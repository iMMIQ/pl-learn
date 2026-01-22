open Ast
open Bytecode

(* {1 Compilation Context} *)

type ctx = {
  mutable env : (string * int) list;
  mutable counter : int;
  mutable closures : (string * string list * expr) list;
}

let empty_ctx = {
  env = [];
  counter = 0;
  closures = [];
}

let fresh_label ctx prefix =
  let label = prefix ^ "_" ^ string_of_int ctx.counter in
  ctx.counter <- ctx.counter + 1;
  (label, ctx)

let extend ctx vars =
  let n = List.length ctx.env in
  ctx.env <- List.map (fun v -> (v, n)) (List.rev vars) @ ctx.env;
  ctx

let lookup ctx var =
  try List.assoc var ctx.env
  with Not_found -> failwith ("Unbound variable: " ^ var)

(* {1 Expression Compilation} *)

let rec compile_expr ctx e = match e with
  | EInt n ->
      (add_instr "" (Const n) [], ctx)

  | EBool b ->
      (add_instr "" (Bool b) [], ctx)

  | EVar x ->
      let depth = lookup ctx x in
      (add_instr "" (Access depth) [], ctx)

  | EAbs (params, body) ->
      (* Create closure entry point *)
      let entry_label = "closure_" ^ string_of_int ctx.counter in
      ctx.counter <- ctx.counter + 1;

      (* Save closure for later compilation *)
      ctx.closures <- (entry_label, params, body) :: ctx.closures;

      (* Generate closure creation instruction *)
      let prog = add_instr "" (Closure (0, List.length params)) [] in
      (* Note: address will be filled in during linking *)
      (prog, ctx)

  | EApp (f, args) ->
      (* Compile function *)
      let (f_code, ctx) = compile_expr ctx f in
      (* Compile arguments in order *)
      let (args_code, ctx) = List.fold_left (fun (prog_acc, ctx) arg ->
        let (code, ctx) = compile_expr ctx arg in
        (prog_acc @ code, ctx)
      ) ([], ctx) args in

      (* Order: args first, then function, then call *)
      let call_instr = add_instr "" (Call (List.length args)) [] in
      (args_code @ f_code @ call_instr, ctx)

  | ELet (x, e1, e2) ->
      (* Compile e1 *)
      let (e1_code, ctx) = compile_expr ctx e1 in
      (* Extend environment with x *)
      let ctx = extend ctx [x] in
      (* Compile e2 *)
      let (e2_code, ctx) = compile_expr ctx e2 in
      (* Combine *)
      (List.rev_append e1_code e2_code, ctx)

  | EPrim (op, e1, e2) ->
      let (e1_code, ctx) = compile_expr ctx e1 in
      let (e2_code, ctx) = compile_expr ctx e2 in
      let bop = match op with
        | Add -> BAdd | Sub -> BSub | Mul -> BMul | Div -> BDiv
        | Eq -> BEq | Lt -> BLt | Gt -> BGt
      in
      let op_instr = add_instr "" (Binop bop) [] in
      (List.rev_append e1_code (List.rev_append e2_code op_instr), ctx)

  | EIf (e1, e2, e3) ->
      let (e1_code, ctx) = compile_expr ctx e1 in
      let else_label = "else_" ^ string_of_int ctx.counter in
      ctx.counter <- ctx.counter + 1;
      let end_label = "end_" ^ string_of_int ctx.counter in
      ctx.counter <- ctx.counter + 1;

      (* Compile then branch *)
      let (then_code, ctx) = compile_expr ctx e2 in

      (* Compile else branch *)
      let (else_code, ctx) = compile_expr ctx e3 in

      let jmp_instr = add_instr "" (Jmp end_label) [] in
      let jmpifz_instr = add_instr "" (JmpIfZ else_label) [] in

      (* Assemble: e1; JmpIfZ else; then_code; Jmp end; else: else_code; end: *)
      let part1 = e1_code @ jmpifz_instr @ then_code @ jmp_instr in
      let part2 = [(else_label, Label else_label)] @ else_code in
      let part3 = [(end_label, Label end_label)] in
      (part1 @ part2 @ part3, ctx)

  | ESeq exprs ->
      List.fold_left (fun (prog, ctx) e ->
        let (code, ctx) = compile_expr ctx e in
        (prog @ code, ctx)
      ) ([], ctx) exprs

(* {1 Compile All Closures} *)

let compile_all_closures ctx prog =
  (* Compile each closure and append to program *)
  List.fold_left (fun prog (entry_label, params, body) ->
    (* Create new environment for closure *)
    let ctx' = { empty_ctx with env = List.map (fun p -> (p, 0)) (List.rev params) } in
    let (body_code, _) = compile_expr ctx' body in
    let return_instr = add_instr "" Return [] in

    (* Add entry point label and body *)
    (entry_label, Label entry_label) ::
    (List.rev_append body_code return_instr) @
    prog
  ) prog ctx.closures

(* {1 Main Compilation Entry} *)

let compile e =
  let ctx = empty_ctx in
  let (main_code, ctx) = compile_expr ctx e in
  let halt_label = "halt_" ^ string_of_int ctx.counter in

  (* Add halt instruction *)
  let main_with_halt = main_code @ [(halt_label, Label halt_label)] in

  (* Compile all closures *)
  let with_closures = compile_all_closures ctx main_with_halt in

  (* Add main entry point *)
  let with_entry = ("main", Label "main") :: with_closures in

  List.rev with_entry
