(* {1 Instruction Type} *)

type binop =
  | BAdd | BSub | BMul | BDiv
  | BEq | BLt | BGt

type instr =
  | Const of int
  | Bool of bool
  | Access of int
  | Closure of int * int
  | Call of int
  | Return
  | Pop
  | Binop of binop
  | Jmp of string                 (* Jump to label *)
  | JmpIfZ of string              (* Jump if zero to label *)
  | Label of string               (* Label marker *)

(* {1 Program Type} *)

type program = (string * instr) list

type resolved_instr =
  | RConst of int
  | RBool of bool
  | RAccess of int
  | RClosure of int * int
  | RCall of int
  | RReturn
  | RPop
  | RBinop of binop
  | RJmp of int
  | RJmpIfZ of int
  | RLabel of string

type constant =
  | CInt of int
  | CBool of bool
  | CClosure of int * string list

type const_pool = constant array

(* {1 Program Construction} *)

let empty_program = []

let add_label lbl prog =
  (lbl, Label lbl) :: prog

let add_instr lbl instr prog =
  (lbl, instr) :: prog

(* {1 Label Resolution} *)

module StringMap = Map.Make (String)

let resolve_labels prog =
  (* First pass: collect label positions *)
  let labels = ref StringMap.empty in
  let rec collect pos = function
    | [] -> ()
    | (lbl, _instr) :: rest ->
        labels := StringMap.add lbl pos !labels;
        collect (pos + 1) rest
  in
  collect 0 prog;

  (* Second pass: generate instruction array, replacing labels with offsets *)
  let instrs = ref [] in
  let rec emit curr = function
    | [] -> ()
    | (_lbl, instr) :: rest ->
        let fix_offset target =
          match StringMap.find_opt target !labels with
          | None -> failwith ("Undefined label: " ^ target)
          | Some addr -> addr - curr - 1  (* Relative offset *)
        in
        let fixed_instr = match instr with
          | Const n -> RConst n
          | Bool b -> RBool b
          | Access d -> RAccess d
          | Closure (addr, n) -> RClosure (addr, n)
          | Call n -> RCall n
          | Return -> RReturn
          | Pop -> RPop
          | Binop op -> RBinop op
          | Jmp target -> RJmp (fix_offset target)
          | JmpIfZ target -> RJmpIfZ (fix_offset target)
          | Label lbl -> RLabel lbl
        in
        (* Keep labels in output for VM to find entry points *)
        instrs := fixed_instr :: !instrs;
        emit (curr + 1) rest
  in
  emit 0 prog;
  Array.of_list (List.rev !instrs)

(* {1 Pretty Printing} *)

let string_of_binop = function
  | BAdd -> "add"
  | BSub -> "sub"
  | BMul -> "mul"
  | BDiv -> "div"
  | BEq -> "eq"
  | BLt -> "lt"
  | BGt -> "gt"

let emit = function
  | Const n -> Printf.sprintf "const %d" n
  | Bool b -> Printf.sprintf "bool %b" b
  | Access d -> Printf.sprintf "access %d" d
  | Closure (addr, n) -> Printf.sprintf "closure %d %d" addr n
  | Call n -> Printf.sprintf "call %d" n
  | Return -> "return"
  | Pop -> "pop"
  | Binop op -> string_of_binop op
  | Jmp lbl -> Printf.sprintf "jmp %s" lbl
  | JmpIfZ lbl -> Printf.sprintf "jmpifz %s" lbl
  | Label lbl -> Printf.sprintf "%s:" lbl

let emit_resolved = function
  | RConst n -> Printf.sprintf "const %d" n
  | RBool b -> Printf.sprintf "bool %b" b
  | RAccess d -> Printf.sprintf "access %d" d
  | RClosure (addr, n) -> Printf.sprintf "closure %d %d" addr n
  | RCall n -> Printf.sprintf "call %d" n
  | RReturn -> "return"
  | RPop -> "pop"
  | RBinop op -> string_of_binop op
  | RJmp offset -> Printf.sprintf "jmp %d" offset
  | RJmpIfZ offset -> Printf.sprintf "jmpifz %d" offset
  | RLabel lbl -> Printf.sprintf "%s:" lbl

let disassemble code =
  let lines = ref [] in
  for i = 0 to Array.length code - 1 do
    lines := Printf.sprintf "% 4d: %s" i (emit_resolved code.(i)) :: !lines
  done;
  String.concat "\n" (List.rev !lines)

(* {1 Constant Pool} *)

let make_const_pool consts =
  Array.of_list consts
