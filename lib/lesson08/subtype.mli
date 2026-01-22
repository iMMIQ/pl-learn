(** Lesson 08: Subtyping *)

(** {1 What is Subtyping?}

    Subtyping is a relation ≤ between types:
    - τ1 ≤ τ2 means "τ1 can be used where τ2 is expected"
    - Also written τ1 <: τ2

    Examples:
    - Int ≤ Top (Int is subtype of Top)
    - {x:Int, y:Int} ≤ {x:Int} (width subtyping)
    - {x:Int} ≤ {x:Top} (depth subtyping)
*)

(** {1 Types} *)

type ty =
  | TyTop                              (* Top type - supertype of all *)
  | TyBot                              (* Bottom type - subtype of all *)
  | TyBool
  | TyInt
  | TyString
  | TyRecord of (string * ty) list     (* Record types: {l1:τ1, l2:τ2, ...} *)
  | TyArrow of ty * ty                 (* Function types: τ1 → τ2 *)

(** {1 Subtype Relation} *)

(** [is_subtype t1 t2] checks if t1 ≤ t2 *)
exception SubtypeError of string

val is_subtype : ty -> ty -> bool

(** [join t1 t2] computes the least upper bound (join) of t1 and t2.
    This is the smallest type that both t1 and t2 are subtypes of.
*)
val join : ty -> ty -> ty

(** [meet t1 t2] computes the greatest lower bound (meet) of t1 and t2.
    This is the largest type that is a subtype of both t1 and t2.
*)
val meet : ty -> ty -> ty

(** {1 Pretty Printing} *)

val string_of_ty : ty -> string
