(** Lesson 02: Lambda Calculus *)

(** {1 Syntax} *)

type expr =
  | Var of string                      (* Variable: x, y, z *)
  | Abs of string * expr               (* Abstraction: <lambda>x. e *)
  | App of expr * expr                 (* Application: e1 e2 *)

(** {1 Free Variables} *)

val free_vars : expr -> string list
val is_closed : expr -> bool

(** {1 Substitution} *)

(** [subst x e body] computes body[x := e] with capture avoidance *)
val subst : string -> expr -> expr -> expr

(** {1 Beta Reduction} *)

type strategy =
  | Normal_order        (* Leftmost-outermost *)
  | Applicative_order   (* Leftmost-innermost *)

val reduce_one : strategy -> expr -> expr option
val normalize : ?strategy:strategy -> ?max_steps:int -> expr -> expr option

(** {1 Church Encodings} *)

val church_true : expr
val church_false : expr
val church_if : expr
val church_pair : expr
val church_fst : expr
val church_snd : expr
val church_zero : expr
val church_succ : expr
val church_add : expr
val church_mul : expr

val church_to_int : expr -> int option
val int_to_church : int -> expr

(** {1 Pretty Printing} *)

val string_of_expr : expr -> string
val pp_expr : Format.formatter -> expr -> unit
