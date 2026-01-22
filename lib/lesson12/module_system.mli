(** Lesson 12: Modules and Functors *)

(** {1 What are Modules?}

    Modules are collections of types, values, and submodules.
    They provide:
    - Namespace management
    - Information hiding (abstraction)
    - Code reuse via functors (parametrized modules)

    OCaml's module system is inspired by Standard ML.
*)

(** {1 Expression and Pattern Types} *)

type typ =
  | TyInt
  | TyBool
  | TyVar of string
  | TyArrow of typ * typ
  | TyName of string

and expr =
  | EConst of int
  | EVar of string
  | ELam of string * typ * expr
  | EApp of expr * expr
  | EMatch of expr * (pattern * expr) list

and pattern =
  | PVar of string
  | PConst of int
  | PWildcard

(** {1 Signatures (Module Types)} *)

(** A signature specifies a module's interface *)
type sig_item =
  | SigType of string * kind option        (* type t (abstract) *)
  | SigTypedef of string * typ             (* type t = ... *)
  | SigVal of string * typ                 (* val x : t *)
  | SigModule of string * signature        (* module M : S *)
  | SigInclude of signature                (* include S *)

and signature = sig_item list

and kind =
  | KStar                                  (* Type *)
  | KArrow of kind * kind                  (* Type constructor *)

(** {1 Structures (Module Implementations)} *)

(** A structure implements a signature *)
type struct_item =
  | StructType of string * typ option      (* type t = ... *)
  | StructVal of string * expr             (* let x = ... *)
  | StructModule of string * structure     (* module M = ... *)
  | StructInclude of structure             (* include ... *)

and structure = struct_item list

(** {1 Module Values} *)

(** Values at the module level *)
type module_value =
  | MVStructure of structure                (* Concrete structure *)
  | MVFunctor of (string * signature) * (structure -> structure)
                                              (* Functor parameter -> body *)

(** {1 Signature Operations} *)

(** [empty_sig] creates an empty signature *)
val empty_sig : signature

(** [add_type name sig] adds abstract type to signature *)
val add_type : string -> signature -> signature

(** [add_val name ty sig] adds value to signature *)
val add_val : string -> typ -> signature -> signature

(** [extend sig1 sig2] combines two signatures *)
val extend : signature -> signature -> signature

(** {1 Structure Operations} *)

(** [empty_struct] creates an empty structure *)
val empty_struct : structure

(** [add_value name expr struct] adds value to structure *)
val add_value : string -> expr -> structure -> structure

(** [add_type_binding name ty struct] adds type binding to structure *)
val add_type_binding : string -> typ option -> structure -> structure

(** {1 Type Checking Modules} *)

exception ModuleError of string

(** [check_structure sig struct] checks that [struct] implements [sig]. *)
val check_structure : signature -> structure -> unit

(** [typecheck_sig env sig] typechecks a signature *)
val typecheck_sig : (string -> typ option) -> signature -> unit

(** {1 Pretty Printing} *)

val string_of_signature : signature -> string
val string_of_structure : structure -> string
val string_of_sig_item : sig_item -> string
val string_of_struct_item : struct_item -> string
