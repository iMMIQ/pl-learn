(** Monad Transformers - Combining Effects *)

(** {1 What are Monad Transformers?}

    Monad transformers allow stacking monads to combine effects.
    Each transformer "wraps" an inner monad and adds a new effect.

    Example: StateT (Result e) = State + Error effects
*)

(** {1 Common Transformer Interface} *)

module type MonadTrans = sig
  type 'a t
  type 'a inner

  (** [lift m] lifts a computation from the inner monad *)
  val lift : 'a inner -> 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val join : 'a t t -> 'a t
end

(** {1 MaybeT (Maybe Transformer) } *)

(** Adds failure capability to any monad *)
module MaybeT : functor (M : Monad.Monad) -> MonadTrans with type 'a inner = 'a M.t

(** {1 StateT (State Transformer) } *)

(** Adds state threading to any monad *)
module StateT : functor (M : Monad.Monad) -> sig
  type 'a t
  type 'a inner = 'a M.t
  type state

  val lift : 'a M.t -> 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t

  val get : state t
  val put : state -> unit t
  val modify : (state -> state) -> unit t
  val run_state : state -> 'a t -> ('a * state) M.t
end

(** {1 ExceptT (Exception Transformer) } *)

(** Adds error handling to any monad *)
module ExceptT : functor (M : Monad.Monad) -> sig
  type 'a t
  type 'a inner = 'a M.t
  type error = string

  val lift : 'a M.t -> 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t

  val throwE : string -> 'a t
  val catchE : 'a t -> (string -> 'a t) -> 'a t
  val run : 'a t -> (string, 'a) Exception.result M.t
end

(** {1 Combined Effects Examples} *)

(** State + Error: computations with state that can fail *)
module StateWithError : sig
  type state
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val throw : string -> 'a t
  val get : state t
  val put : state -> unit t

  val run : state -> 'a t -> (string, 'a * state) Exception.result
end

(** State + IO (simulated): stateful computations with logging *)
module StateWithLog : sig
  type state
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val log : string -> unit t
  val get : state t
  val put : state -> unit t

  val run : state -> 'a t -> 'a * state * string list
end
