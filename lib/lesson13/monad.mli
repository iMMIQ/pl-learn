(** Lesson 13: Effects and Monads *)

(** {1 What are Monads?}

    A monad is a design pattern for structuring programs with effects.
    It provides:
    - A way to wrap values: return
    - A way to sequence computations: bind

    Monads model "computations that produce values" rather than just values.
    This allows pure functional languages to handle effects like:
    - State mutation
    - Exceptions
    - I/O
    - Non-determinism
    - Continuations
*)

(** {1 Monad Typeclass} *)

(** A monad has two operations: return and bind *)
module type Monad = sig
  type 'a t  (* The computation type *)

  (** [return x] lifts a pure value into the monad *)
  val return : 'a -> 'a t

  (** [bind m f] sequences computations: runs [m], then applies [f] to result *)
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (** [map f m] applies a pure function to a monadic value *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** [join m] flattens a nested monad: 'a t t -> 'a t *)
  val join : 'a t t -> 'a t
end

(** {1 Monad Laws} *)

(** The three monad laws that all instances must satisfy:
    1. Left identity:  bind (return x) f  ==  f x
    2. Right identity: bind m return     ==  m
    3. Associativity: bind (bind m f) g  ==  bind m (fun x -> bind (f x) g)
*)

(** {1 Monad Operations} *)

(** These operations are provided as functors for specific monad instances.
   See the MonadUtils functor for generic implementations. *)

(** {1 Generic Monad Functions} *)

(** These work for any monad instance *)
module MonadUtils (M : Monad) : sig
  val mapM : ('a -> 'b M.t) -> 'a list -> 'b list M.t
  val filterM : ('a -> bool M.t) -> 'a list -> 'a list M.t
  val foldM : ('a -> 'b -> 'a M.t) -> 'a -> 'b list -> 'a M.t
  val sequence : 'a M.t list -> 'a list M.t
  val replicateM : int -> 'a M.t -> 'a list M.t
end

(** {1 Monad Laws Verification} *)

(** Monad laws verification is a placeholder for property-based testing.
   In practice, use QuickCheck or similar to verify laws for specific instances. *)
