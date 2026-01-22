(** Pattern Matching *)

(** {1 Pattern Type} *)

type pattern =
  | PWildcard                            (* _ - matches anything *)
  | PVar of string                       (* x - binds variable *)
  | PConst of Adt.value                  (* 42, true, "hello" *)
  | PTuple of pattern list               (* (p1, p2, ..., pn) *)
  | PRecord of (string * pattern) list   (* {x=p1; y=p2} *)
  | PVariant of string * pattern option  (* C(p) or C *)
  | POr of pattern * pattern             (* p1 | p2 - OR pattern *)
  | PAs of string * pattern              (* x as p - named pattern *)

(** {1 Internal Type for Exhaustiveness Checking} *)

type typ =
  | TBool
  | TInt
  | TUnit
  | TVariant of (string * typ option) list
  | TTuple of typ list

(** {1 Decision Tree Type} *)

type decision_tree =
  | DLeaf of string list                  (* Success with variable bindings *)
  | DFail                                 (* No match *)
  | DSwitch of string * (string * decision_tree) list * decision_tree
    (* Switch on variant: variable -> cases -> default *)

(** {1 Matching} *)

(** [match_value p v] attempts to match value [v] against pattern [p].
    Returns Some(env) with bindings if match succeeds, None otherwise.
*)
val match_value : pattern -> Adt.value -> (string * Adt.value) list option

(** [matches p v] tests if pattern [p] matches value [v]. *)
val matches : pattern -> Adt.value -> bool

(** {1 Pattern Compilation} *)

(** [compile_pattern p] compiles a pattern to a decision tree. *)
val compile_pattern : pattern -> decision_tree

(** [compile_patterns ps] compiles a list of patterns into a single decision tree. *)
val compile_patterns : (pattern * Adt.expr) list -> decision_tree

(** [eval_decision_tree dt env v] evaluates decision tree [dt] with environment [env] and value [v]. *)
val eval_decision_tree : decision_tree -> Adt.value Adt.env -> Adt.value -> Adt.expr option

(** {1 Exhaustiveness Checking} *)

(** [is_exhaustive ps] checks if patterns [ps] cover all possible values. *)
val is_exhaustive : pattern list -> typ -> bool

(** [missing_patterns ps ty] returns patterns not covered by [ps] for type [ty]. *)
val missing_patterns : pattern list -> typ -> pattern list option

(** {1 Pretty Printing} *)

val string_of_pattern : pattern -> string
val string_of_decision_tree : decision_tree -> string
