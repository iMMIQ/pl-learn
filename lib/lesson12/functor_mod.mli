(** Functors - Functions over Modules *)

open Module_system

(** {1 Functor Type} *)

(** A functor is a function from modules to modules *)
type functor_def = {
  fname : string;                           (* Functor name *)
  fparam : string * signature;              (* (Parameter name, signature) *)
  fbody : structure;                        (* Result structure *)
}

(** {1 Functor Application} *)

(** [apply_functor functor arg] applies functor to argument module.
    Returns the resulting structure.
*)
val apply_functor : functor_def -> structure -> structure

(** [instantiate_structure name struct] substitutes module references in structure.
    When a functor is applied, references to the parameter module are replaced
    with the actual argument structure.
*)
val instantiate_structure : string -> structure -> structure -> structure

(** {1 Functor Type Checking} *)

(** [check_functor functor] checks that functor is well-formed. *)
val check_functor : functor_def -> unit

(** [check_functor_application functor arg_sig] checks that argument signature
    matches functor parameter signature. *)
val check_functor_application : functor_def -> signature -> unit

(** {1 Higher-Order Functors} *)

(** [compose f1 f2] creates a functor that applies f1 then f2. *)
val compose : functor_def -> functor_def -> functor_def

(** {1 Pretty Printing} *)

val string_of_functor : functor_def -> string
