(* {1 Extended CPS Types with First-Class Continuations} *)

type cps_cont_ext =
  | EHalt
  | EArg of string * cps_cont_ext
  | EBin1 of string * cps_expr_ext * cps_cont_ext
  | EFunc of cps_expr_ext * cps_cont_ext

and cps_expr_ext =
  | EConst of int
  | EVar of string
  | EPrim of string * cps_expr_ext list * cps_cont_ext
  | ELet of string * cps_expr_ext * cps_cont_ext
  | ECallCC of string * cps_cont_ext

(* {1 Value Type} *)

type value =
  | VInt of int
  | VCont of cps_cont_ext

(* {1 Evaluation} *)

let rec eval_ext env = function
  | EConst n -> VInt n
  | EVar x ->
      (try env x with Not_found ->
         raise (Invalid_argument ("Unbound: " ^ x)))

  | EPrim (op, args, k) ->
      let values = List.map (eval_ext env) args in
      let result = match op, values with
        | "+", [VInt n1; VInt n2] -> VInt (n1 + n2)
        | "*", [VInt n1; VInt n2] -> VInt (n1 * n2)
        | _ -> raise (Invalid_argument "Unknown op")
      in
      eval_cont_ext env k result

  | ELet (x, e1, k) ->
      let v1 = eval_ext env e1 in
      let env' y = if y = x then v1 else env y in
      eval_cont_ext env' k v1

  | ECallCC (x, k) ->
      (* Call/cc: bind x to current continuation, then evaluate under k *)
      let env' y = if y = x then VCont k else env y in
      eval_cont_ext env' k (VInt 0) (* Placeholder value *)

and eval_cont_ext env k v =
  match k with
  | EHalt -> v
  | EArg (x, k') ->
      let env' y = if y = x then v else env y in
      eval_cont_ext env' k' v
  | EBin1 (op, e2, k') ->
      let v2 = eval_ext env e2 in
      let result = match v, v2 with
        | VInt n1, VInt n2 ->
            (match op with
             | "+" -> VInt (n1 + n2)
             | "*" -> VInt (n1 * n2)
             | _ -> raise (Invalid_argument "Unknown op"))
        | _ -> raise (Invalid_argument "Type error")
      in
      eval_cont_ext env k' result
  | EFunc (f, k') ->
      (* Apply function to value *)
      eval_cont_ext env k' (eval_ext env f)

(* {1 Examples} *)

(* Using call/cc to return early *)
(* Conceptually: (call/cc (λk. (+ 1 (k 2)))) returns 2 *)
let example_callcc =
  ECallCC ("k",
    EFunc (EPrim ("+", [EConst 1; EVar "k"], EHalt),
      EArg ("_", EHalt)))

(* Exception-like behavior with continuations *)
(* Conceptually: (throw 5) catches the value 5 *)
let example_exception =
  EPrim ("throw", [EConst 5], EHalt) (* Would need proper throw/catch setup *)
