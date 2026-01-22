(** Bytecode Instruction Set *)

(** {1 Instruction Type} *)

(** Each instruction is tagged with source position for error reporting.
    Jump instructions use string labels that are resolved during compilation.
*)
type instr =
  | Const of int                     (* Push integer constant *)
  | Bool of bool                     (* Push boolean constant *)
  | Access of int                    (* Access variable at depth *)
  | Closure of int * int             (* Create closure (addr, num_vars) *)
  | Call of int                      (* Call function with n args *)
  | Return                           (* Return from function *)
  | Pop                              (* Pop value from stack *)
  | Binop of binop                   (* Binary operation *)
  | Jmp of string                    (* Unconditional jump to label *)
  | JmpIfZ of string                 (* Jump if top of stack is zero to label *)
  | Label of string                  (* Label for jumping (for display) *)

and binop =
  | BAdd | BSub | BMul | BDiv
  | BEq | BLt | BGt

(** {1 Code Representation} *)

(** A program is a list of labeled instructions *)
type program = (string * instr) list

(** Resolved instructions (after label resolution) *)
type resolved_instr =
  | RConst of int
  | RBool of bool
  | RAccess of int
  | RClosure of int * int
  | RCall of int
  | RReturn
  | RPop
  | RBinop of binop
  | RJmp of int                     (* Relative offset *)
  | RJmpIfZ of int                  (* Relative offset *)
  | RLabel of string

(** {1 Bytecode Utilities} *)

val empty_program : program

val add_label : string -> program -> program
val add_instr : string -> instr -> program -> program

(** [resolve_labels prog] converts labels to absolute addresses *)
val resolve_labels : program -> resolved_instr array

(** [emit instr] formats an instruction as a string *)
val emit : instr -> string

(** [disassemble code] prints bytecode as human-readable text *)
val disassemble : resolved_instr array -> string

(** {1 Constant Pool} *)

(** Constants stored separately from instructions *)
type constant =
  | CInt of int
  | CBool of bool
  | CClosure of int * string list

type const_pool = constant array

val make_const_pool : constant list -> const_pool
