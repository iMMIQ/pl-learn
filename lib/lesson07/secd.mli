(** Lesson 07: SECD Machine *)

(** {1 What is SECD?}

    The SECD machine is a virtual machine for lambda calculus.
    Name comes from its four registers:

    - S: Stack - intermediate values
    - E: Environment - variable bindings
    - C: Control - remaining code
    - D: Dump - saved states (for function return)
*)

(** {1 Abstract Syntax} *)

type expr =
  | Var of string
  | Abs of string * expr
  | App of expr * expr
  | Const of int
  | Add of expr * expr
  | Let of string * expr * expr

(** {1 Instructions} *)

(** The machine operates on instructions, not direct expressions. *)
type instr =
  | IConst of int                      (* Push constant *)
  | IAccess of int                     (* Access variable by depth *)
  | IClose of instr list               (* Create closure *)
  | IApp                               (* Apply function *)
  | IReturn                            (* Return from function *)
  | IAdd                               (* Add two values *)
  | ILet of instr list * instr list    (* Let binding *)

(** {1 Values} *)

type value =
  | VInt of int
  | VClosure of string * instr list * value array

(** {1 Machine State} *)

type secd = {
  stack : value list;
  env : value array list;              (* Environment as list of frames *)
  control : instr list;
  dump : secd list;                    (* Saved states *)
}

(** {1 Compilation} *)

val compile : expr -> instr list

(** {1 Execution} *)

val init : expr -> secd
val step : secd -> secd
val run : secd -> value

(** {1 Pretty Printing} *)

val string_of_value : value -> string
val string_of_instr : instr -> string
val string_of_state : secd -> string
