(** Lesson 11: Algebraic Data Types *)

(** {1 What are Algebraic Data Types?}

    Algebraic Data Types (ADTs) are composite types formed by:
    - Product types: combining values (tuples, records)
    - Sum types: choosing one variant (tagged unions)
    - Recursive types: types that refer to themselves

    "Algebraic" refers to algebra of types:
    - a × b is product type (both a AND b)
    - a + b is sum type (either a OR b)
*)

(** {1 Type Definitions} *)

(** Base and composite types *)
type typ =
  | TyBool                                 (* Bool *)
  | TyInt                                  (* Int *)
  | TyString                               (* String *)
  | TyUnit                                 (* Unit - () *)
  | TyTuple of typ list                    (* Product type *)
  | TyRecord of (string * typ) list        (* Record type *)
  | TyVariant of (string * typ option) list (* Sum type *)
  | TyVar of string                        (* Type variable *)
  |TyArrow of typ * typ                    (* Function type *)
  | TyName of string                       (* Named type reference *)

(** Type constructors for defining ADTs *)
type typedef =
  | TTuple of typ list                    (* (t1 * t2 * ... * tn) *)
  | TRecord of (string * typ) list        (* {x1: t1; ...; xn: tn} *)
  | TVariant of (string * typ option) list (* [| C1 of t1; ...; Cn of tn |] *)
  | TAlias of string * typ                 (* type t = ... *)

(** {1 Values} *)

and value =
  | VBool of bool
  | VInt of int
  | VString of string
  | VUnit
  | VTuple of value list
  | VRecord of (string * value) list
  | VVariant of string * value option      (* Constructor name, optional payload *)
  | VClosure of string * expr * value env

and expr =
  | EConst of value
  | EVar of string
  | ETuple of expr list
  | ERecord of (string * expr) list
  | EField of expr * string                (* e.field *)
  | EConstruct of string * expr option     (* C(e) or C *)
  | EMatch of expr * match_rule list
  | EAbs of string * typ * expr
  | EApp of expr * expr
  | ELet of string * expr * expr

and match_rule = string list * expr       (* (pattern, guard) -> body *)

and 'a env = (string -> 'a option)

(** {1 Type Operations} *)

(** [arrow t1 t2] creates function type t1 → t2 *)
val arrow : typ -> typ -> typ

(** [tuple ts] creates tuple type *)
val tuple : typ list -> typ

(** [variant cases] creates variant type *)
val variant : (string * typ option) list -> typ

(** [record fields] creates record type *)
val record : (string * typ) list -> typ

(** {1 Type Checking} *)

exception TypeError of string

(** [typeof env e] infers type of expression *)
val typeof : (string -> typ option) -> expr -> typ

(** [check_type env e ty] checks that [e] has type [ty] *)
val check_type : (string -> typ option) -> expr -> typ -> unit

(** {1 Pretty Printing} *)

val string_of_typ : typ -> string
val string_of_value : value -> string
val string_of_expr : expr -> string
