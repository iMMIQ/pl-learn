(** Type Inference for Mini-ML *)

open Types

(** {1 Expressions} *)

type expr =
  | EConst of int                       (* Integer constant *)
  | EBool of bool                       (* Boolean constant *)
  | EVar of string                      (* Variable *)
  | EAbs of string * expr               (* Function: λx. e *)
  | EApp of expr * expr                 (* Application: e1 e2 *)
  | ELet of string * expr * expr        (* Let binding *)
  | EIf of expr * expr * expr           (* Conditional *)
  | EBinOp of string * expr * expr      (* Binary operators *)

(** {1 Type Schemes} *)

(** A type scheme represents a polymorphic type: ∀α1...αn. τ
    For example: 'a -> 'a represents ∀'a. 'a → 'a
*)
type scheme =
  | Mono of ty                          (* Monomorphic type *)
  | Poly of string list * ty            (* Polymorphic: ∀vars. ty *)

(** {1 Type Environment} *)

type env = (string * scheme) list

(** {1 Inference} *)

exception TypeError of string

val fresh : unit -> ty
val fresh_var : unit -> string

(** [infer env expr] infers the type of [expr] in environment [env]. *)
val infer : env -> expr -> ty * substitution

(** [typeof expr] infers the type of [expr] in empty environment. *)
val typeof : expr -> ty

(** {1 Generalization and Instantiation} *)

(** [generalize env ty] generalizes type [ty] over variables not in [env]. *)
val generalize : env -> ty -> scheme

(** [instantiate scheme] creates fresh instances of type variables. *)
val instantiate : scheme -> ty

(** {1 Pretty Printing} *)

val string_of_scheme : scheme -> string
val string_of_expr : expr -> string
