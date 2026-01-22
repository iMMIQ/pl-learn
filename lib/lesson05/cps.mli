(** Lesson 05: Continuations and CPS *)

(** {1 Source Language - Direct Style} *)

type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr
  | Var of string
  | Let of string * expr * expr

(** {1 CPS Language} *)

type cps_expr =
  | CConst of int
  | CVar of string
  | CPrim of string * cps_expr list
  | CLet of string * cps_expr * cps_expr

(** {1 CPS Transformation} *)

val cps_transform : expr -> cps_expr

(** {1 CPS Evaluation} *)

val eval_cps : (string -> int) -> cps_expr -> int

(** {1 Pretty Printing} *)

val string_of_cps_expr : cps_expr -> string
