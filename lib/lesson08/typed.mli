(** Typed Expressions with Subtyping *)

type ty = Subtype.ty =
  | TyTop
  | TyBot
  | TyBool
  | TyInt
  | TyString
  | TyRecord of (string * ty) list
  | TyArrow of ty * ty

(** {1 Expressions} *)

type expr =
  | EConst of int
  | EBool of bool
  | EString of string
  | EVar of string
  | EAbs of string * ty * expr           (* λx:τ. e *)
  | EApp of expr * expr
  | ERecord of (string * expr) list      (* {l1=e1, l2=e2, ...} *)
  | EProj of expr * string               (* e.l *)
  | ELet of string * expr * expr

(** {1 Type Environment} *)

type env = (string * ty) list

(** {1 Type Checking with Subtyping} *)

(** [typeof env e] computes the type of expression e in environment env.
    @raise Subtype.SubtypeError if type checking fails
*)
val typeof : env -> expr -> ty

(** [typecheck e] computes the type of expression e in empty environment.
    @raise Subtype.SubtypeError if type checking fails
*)
val typecheck : expr -> ty

(** {1 Subsumption} *)

(** [subsumption ty expected] verifies ty ≤ expected and returns expected.
    @raise Subtype.SubtypeError if ty is not a subtype of expected
*)
val subsumption : ty -> ty -> ty

(** {1 Pretty Printing} *)

val string_of_expr : expr -> string
