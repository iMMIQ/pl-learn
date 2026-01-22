open Ast
open Bytecode

(* {1 VM State} *)

type vm = {
  mutable code : resolved_instr array;
  mutable pc : int;
  mutable stack : value array;
  mutable sp : int;
  mutable closures : value array array;
  mutable tracing : bool;
}

let stack_size = 1000
let max_closures = 100

(* {1 VM Creation} *)

let create_vm code = {
  code = code;
  pc = 0;
  stack = Array.make stack_size VUnit;
  sp = 0;
  closures = Array.make max_closures [||];
  tracing = false;
}

let reset vm =
  vm.pc <- 0;
  vm.sp <- 0;
  for i = 0 to Array.length vm.stack - 1 do
    vm.stack.(i) <- VUnit
  done

(* {1 Stack Operations} *)

let push vm v =
  if vm.sp >= stack_size then
    failwith "Stack overflow";
  vm.stack.(vm.sp) <- v;
  vm.sp <- vm.sp + 1

let pop vm =
  if vm.sp <= 0 then failwith "Stack underflow";
  vm.sp <- vm.sp - 1;
  vm.stack.(vm.sp)

let peek vm n =
  if vm.sp - 1 - n < 0 then failwith "Stack underflow";
  vm.stack.(vm.sp - 1 - n)

(* {1 Instruction Execution} *)

let exec_binop vm = function
  | BAdd ->
      let v2 = pop vm in
      let v1 = pop vm in
      (match v1, v2 with
       | VInt n1, VInt n2 -> push vm (VInt (n1 + n2))
       | _ -> failwith "Type error in +")
  | BSub ->
      let v2 = pop vm in
      let v1 = pop vm in
      (match v1, v2 with
       | VInt n1, VInt n2 -> push vm (VInt (n1 - n2))
       | _ -> failwith "Type error in -")
  | BMul ->
      let v2 = pop vm in
      let v1 = pop vm in
      (match v1, v2 with
       | VInt n1, VInt n2 -> push vm (VInt (n1 * n2))
       | _ -> failwith "Type error in *")
  | BDiv ->
      let v2 = pop vm in
      let v1 = pop vm in
      (match v1, v2 with
       | VInt n1, VInt n2 ->
           if n2 = 0 then failwith "Division by zero"
           else push vm (VInt (n1 / n2))
       | _ -> failwith "Type error in /")
  | BEq ->
      let v2 = pop vm in
      let v1 = pop vm in
      (match v1, v2 with
       | VInt n1, VInt n2 -> push vm (VBool (n1 = n2))
       | VBool b1, VBool b2 -> push vm (VBool (b1 = b2))
       | _ -> failwith "Type error in =")
  | BLt ->
      let v2 = pop vm in
      let v1 = pop vm in
      (match v1, v2 with
       | VInt n1, VInt n2 -> push vm (VBool (n1 < n2))
       | _ -> failwith "Type error in <")
  | BGt ->
      let v2 = pop vm in
      let v1 = pop vm in
      (match v1, v2 with
       | VInt n1, VInt n2 -> push vm (VBool (n1 > n2))
       | _ -> failwith "Type error in >")

let step vm =
  if vm.pc < 0 || vm.pc >= Array.length vm.code then
    false
  else
    let instr = vm.code.(vm.pc) in
    if vm.tracing then
      Printf.printf "[%d] %s\n" vm.pc (Bytecode.emit_resolved instr);
    (match instr with
     | RConst n ->
         push vm (VInt n);
         vm.pc <- vm.pc + 1
     | RBool b ->
         push vm (VBool b);
         vm.pc <- vm.pc + 1
     | RAccess depth ->
         push vm (peek vm depth);
         vm.pc <- vm.pc + 1
     | RClosure (addr, n) ->
         (* Capture current environment *)
         let _env = Array.sub vm.stack (vm.sp - n) n in
         push vm (VClosure (addr, []));  (* Simplified: env not stored *)
         vm.sp <- vm.sp - n;
         vm.pc <- vm.pc + 1
     | RCall n ->
         (* Pop function closure *)
         let closure = pop vm in
         (match closure with
          | VClosure (addr, _params) ->
              (* Remove args from current frame *)
              for _i = 0 to n - 1 do
                ignore (pop vm)
              done;
              (* Jump to function *)
              vm.pc <- addr;
          | _ -> failwith "Cannot apply non-function")
     | RReturn ->
         (* Return to caller *)
         vm.pc <- vm.pc + 1  (* Simplified: just halt *)
     | RPop ->
         ignore (pop vm);
         vm.pc <- vm.pc + 1
     | RBinop op ->
         exec_binop vm op;
         vm.pc <- vm.pc + 1
     | RJmp offset ->
         vm.pc <- vm.pc + offset + 1
     | RJmpIfZ offset ->
         let v = pop vm in
         (match v with
          | VBool false | VInt 0 -> vm.pc <- vm.pc + offset + 1
          | _ -> vm.pc <- vm.pc + 1)
     | RLabel _ ->
         vm.pc <- vm.pc + 1);
    true

let run vm =
  while step vm do
    ()
  done;
  if vm.sp > 0 then pop vm else VUnit

let run_from_entry vm entry =
  (* Find entry point *)
  let rec find_entry addr =
    if addr >= Array.length vm.code then
      failwith ("Entry point not found: " ^ entry)
    else
      match vm.code.(addr) with
      | RLabel lbl when lbl = entry ->
          vm.pc <- addr + 1;
          run vm
      | RLabel _ -> find_entry (addr + 1)
      | _ -> find_entry (addr + 1)
  in
  find_entry 0

let trace vm =
  vm.tracing <- true;
  ()

let dump_state vm =
  Printf.printf "PC: %d\n" vm.pc;
  Printf.printf "Stack (%d):\n" vm.sp;
  for i = 0 to vm.sp - 1 do
    Printf.printf "  [%d] %s\n" i (string_of_value vm.stack.(i))
  done

let disassemble vm =
  Bytecode.disassemble vm.code
