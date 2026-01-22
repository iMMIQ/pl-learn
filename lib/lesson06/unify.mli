(** Unification Algorithm *)

open Types

(** [unify t1 t2] computes a substitution [s] such that s(t1) = s(t2).
    Raises [Unify_error] if types cannot be unified.
*)

exception Unify_error of string * ty * ty

val unify : ty -> ty -> substitution
