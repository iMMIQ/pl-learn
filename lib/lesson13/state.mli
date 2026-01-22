(** State Monad - Pure Functional State *)

(** {1 What is the State Monad?}

    The state monad models computations that read and write state.
    Instead of using mutable variables, we thread state through computations.

    Type: ('s, 'a) t = 's -> 'a * 's
    A computation that takes initial state 's and returns (value, final state)
*)

(** {1 State Type} *)

type ('s, 'a) t

(** The state representation: a function from initial state to (result, final state) *)

(** {1 Monad Operations} *)

val return : 'a -> ('s, 'a) t
val bind : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
val map : ('a -> 'b) -> ('s, 'a) t -> ('s, 'b) t
val join : ('s, ('s, 'a) t) t -> ('s, 'a) t

(** {1 State-Specific Operations} *)

(** [get] reads the current state *)
val get : ('s, 's) t

(** [put s] sets the state to [s] *)
val put : 's -> ('s, unit) t

(** [modify f] transforms the state with function [f] *)
val modify : ('s -> 's) -> ('s, unit) t

(** [gets f] reads and projects the state with [f] *)
val gets : ('s -> 'a) -> ('s, 'a) t

(** [run_state init m] runs the state computation with initial state [init] *)
val run_state : 's -> ('s, 'a) t -> 'a * 's

(** [eval_state init m] runs the computation and returns only the result *)
val eval_state : 's -> ('s, 'a) t -> 'a

(** [exec_state init m] runs the computation and returns only the final state *)
val exec_state : 's -> ('s, 'a) t -> 's

(** {1 Common State Patterns} *)

(** [counter ()] creates a computation that increments a counter (requires int state) *)
val counter : unit -> (int, int) t

(** [sum nums] accumulates integers into a running sum (requires int state) *)
val sum : int list -> (int, unit) t

(** [collect items] collects values into a list (requires list state) *)
val collect : 'a list -> ('a list, unit) t

(** {1 Examples} *)

(** Example: Stack operations *)
type 'a stack = 'a list

(** [push x] pushes x onto the stack (reads/writes stack from state) *)
val push : 'a -> ('a stack, unit) t

(** [pop ()] pops from the stack *)
val pop : unit -> ('a stack, 'a option) t

(** [peek ()] looks at the top of the stack *)
val peek : unit -> ('a stack, 'a option) t

(** Example: Tagged values (fresh name generation)
    Note: fresh uses a mutable reference counter, not state threading *)
val fresh : unit -> ('s, string) t

(** [fresh_pure] uses state threading (requires int state) *)
val fresh_pure : unit -> (int, string) t
