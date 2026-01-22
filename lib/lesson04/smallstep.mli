(** Lesson 04: Small-Step Operational Semantics *)

(** {1 Abstract Syntax} *)

type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr
  | Var of string
  | Let of string * expr * expr

(** {1 Values} *)

type value =
  | VInt of int

(** {1 Single Step Reduction} *)

(** [step e] computes one small-step reduction.
    Returns [Some e'] if e → e', [None] if e is a value.
*)
val step : expr -> expr option

(** {1 Multi-Step Reduction} *)

(** [steps e] returns the complete reduction sequence to normal form. *)
val steps : expr -> expr list

(** [reduce e] computes the normal form using small-step semantics. *)
val reduce : ?max_steps:int -> expr -> expr option

(** {1 Trace Visualization} *)

val trace : expr -> string
