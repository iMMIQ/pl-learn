(** Environment-based Abstract Machine *)

(** {1 Machine Design}

    This machine uses:
    - C: Control (instruction stream)
    - E: Environment (explicit closure-based)
    - S: Stack (for intermediate values)

    Simpler than SECD but more modern in design.
*)

(** {1 Instructions} *)

type instr =
  | IConst of int
  | IVar of string
  | IAbs of string * instr list
  | IApp
  | IAdd
  | ILet of string * instr list * instr list

(** {1 Closure} *)

type closure = {
  param : string;
  body : instr list;
  env : (string * value) list;
}

and value =
  | VInt of int
  | VClos of closure

(** {1 Machine State} *)

type state = {
  code : instr list;
  env : (string * value) list;
  stack : value list;
}

(** {1 Compilation} *)

val compile : Secd.expr -> instr list

(** {1 Execution} *)

val init_state : Secd.expr -> state
val step_state : state -> state
val eval : Secd.expr -> value

(** {1 Tracing} *)

val trace : Secd.expr -> string
