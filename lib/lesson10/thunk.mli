(** Lesson 10: Lazy Evaluation - Thunks *)

(** {1 What is Lazy Evaluation?}

    Lazy evaluation delays computation until a value is actually needed.
    This is different from strict (eager) evaluation:

    - Strict: evaluate arguments before calling function
    - Lazy: evaluate arguments only when their values are needed

    Call-by-need = lazy + memoization (evaluate once, cache result)
*)

(** {1 Thunk Type} *)

(** A thunk is a delayed computation. It can be:
    - Unevaluated: contains a computation to run
    - Evaluating: currently being computed (for detecting cycles)
    - Evaluated: contains the computed result
*)
type 'a thunk =
  | Unevaluated of (unit -> 'a)  (* Not yet computed *)
  | Evaluating                   (* Currently computing (cycle detection) *)
  | Evaluated of 'a              (* Already computed (memoized) *)

(** {1 Thunk Operations} *)

(** [delay f] creates a thunk from computation [f]. *)
val delay : (unit -> 'a) -> 'a thunk

(** [force t] evaluates the thunk if not already evaluated.
    Returns the memoized value if already computed.
*)
val force : 'a thunk -> 'a

(** [is_evaluated t] checks if thunk has been evaluated. *)
val is_evaluated : 'a thunk -> bool

(** [reset t] clears the memoized value (for testing). *)
val reset : 'a thunk -> unit

(** {1 Lazy Value Type} *)

(** Abstract type for lazy values with safe interface *)
type 'a lazy_value

(** [make_lazy f] creates a new lazy value. *)
val make_lazy : (unit -> 'a) -> 'a lazy_value

(** [get lv] forces evaluation of lazy value. *)
val get : 'a lazy_value -> 'a

(** [status lv] returns the current status. *)
val status : 'a lazy_value -> [ `Unevaluated | `Evaluating | `Evaluated ]

(** {1 Pretty Printing} *)

val string_of_status : 'a lazy_value -> string
