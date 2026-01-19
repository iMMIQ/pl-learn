(** Lesson 01: Arithmetic Expressions *)

(** {1 Abstract Syntax} *)

type expr =
  | Const of int                      (** Integer constant *)
  | Add of expr * expr                (** Addition *)
  | Mul of expr * expr                (** Multiplication *)
  | Var of string                     (** Variable reference *)
  | Let of string * expr * expr       (** Let binding *)

(** {1 Evaluation} *)

val eval : (string -> int) -> expr -> int
val run : expr -> int

(** {1 Pretty Printing} *)

val string_of_expr : expr -> string
val pp_expr : Format.formatter -> expr -> unit
