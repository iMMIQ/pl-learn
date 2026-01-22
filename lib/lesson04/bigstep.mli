(** Lesson 04: Big-Step Operational Semantics *)

(** {1 Abstract Syntax} *)

type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr
  | Var of string
  | Let of string * expr * expr

(** {1 Big-Step Evaluation} *)

(** [eval env e] computes the value of expression [e] using big-step semantics.
    The relation is written: env ⊢ e ⇓ v
*)
val eval : (string -> int) -> expr -> int

(** {1 Evaluation derivation tree} *)

type derivation =
  | DConst of int
  | DAdd of derivation * derivation * int
  | DMul of derivation * derivation * int
  | DVar of string * int
  | DLet of string * derivation * derivation * int

(** [eval_derivation env e] computes both value and derivation tree. *)
val eval_derivation : (string -> int) -> expr -> int * derivation

(** {1 Pretty Print Derivation} *)

val pp_derivation : Format.formatter -> derivation -> unit
val string_of_derivation : derivation -> string
