(** Lesson 06: Type Inference - Type Variables and Substitutions *)

(** {1 Types with Variables} *)

type ty =
  | TyVar of string                     (* Type variable: 'a, 'b *)
  | TyBool                              (* Boolean type *)
  | TyInt                               (* Integer type *)
  | TyArrow of ty * ty                  (* Function type: τ1 → τ2 *)

(** {1 Type Substitutions} *)

(** A substitution maps type variables to types.
    Written as: ['a := τ1, 'b := τ2]
*)
type substitution = (string * ty) list

(** [compose s1 s2] applies s1 then s2.
    Written: s1 ∘ s2
*)
val compose : substitution -> substitution -> substitution

(** [apply_subst s ty] applies substitution [s] to type [ty].
    Written: s(τ)
*)
val apply_subst : substitution -> ty -> ty

(** [dom s] returns the domain (variables bound) of substitution [s]. *)
val dom : substitution -> string list

(** [empty_subst] is the empty substitution. *)
val empty_subst : substitution

(** [single v ty] creates substitution ['v := ty]. *)
val single : string -> ty -> substitution

(** [occurs_check v ty] checks if variable [v] occurs in type [ty]. *)
val occurs_check : string -> ty -> bool

(** {1 Pretty Printing} *)

val string_of_ty : ty -> string
val string_of_subst : substitution -> string
