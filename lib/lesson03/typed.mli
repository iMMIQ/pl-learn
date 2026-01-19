(** Lesson 03: Simply Typed Lambda Calculus *)

(** {1 Types} *)

type typ =
  | TyBool                            (* Boolean type *)
  | TyNat                             (* Natural number type *)
  | TyArrow of typ * typ              (* Function type: τ1 → τ2 *)

(** {1 Typed Expressions} *)

type expr =
  | TmVar of string * typ             (* Variable with type *)
  | TmAbs of string * typ * expr      (* Abstraction: λx:τ. e *)
  | TmApp of expr * expr              (* Application: e1 e2 *)
  | TmTrue                            (* true *)
  | TmFalse                           (* false *)
  | TmIf of expr * expr * expr        (* if e1 then e2 else e3 *)
  | TmZero                            (* 0 *)
  | TSucc of expr                     (* succ e *)
  | TPred of expr                     (* pred e *)
  | TmIsZero of expr                  (* iszero e *)

(** {1 Type Checking} *)

exception TypeError of string

val typeof : (string -> typ option) -> expr -> typ
val typecheck : expr -> typ

(** {1 Evaluation} *)

val eval : expr -> expr

(** {1 Pretty Printing} *)

val string_of_typ : typ -> string
val string_of_expr : expr -> string
