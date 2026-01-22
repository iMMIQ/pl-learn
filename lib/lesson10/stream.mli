(** Infinite Streams using Lazy Evaluation *)

(** {1 Stream Type} *)

(** A stream is either:
    - Empty: Nil
    - Cons cell with head (lazy) and tail (lazy stream)
*)
type 'a stream =
  | Nil
  | Cons of 'a * 'a stream lazy_cell

(** A lazy cell for delayed stream computation *)
and 'a lazy_cell = {
  mutable state : 'a lazy_state;
}

and 'a lazy_state =
  | LUnevaluated of (unit -> 'a)
  | LEvaluating
  | LEvaluated of 'a

(** {1 Stream Construction} *)

(** [cons x xs] creates a stream with head [x] and tail [xs].
    The tail is lazy (not evaluated until needed). *)
val cons : 'a -> 'a stream -> 'a stream

(** [nil] is the empty stream. *)
val nil : 'a stream

(** {1 Stream Operations} *)

(** [take n s] returns the first [n] elements of stream [s]. *)
val take : int -> 'a stream -> 'a list

(** [drop n s] skips the first [n] elements of stream [s]. *)
val drop : int -> 'a stream -> 'a stream

(** [map f s] applies function [f] to each element of [s]. *)
val map : ('a -> 'b) -> 'a stream -> 'b stream

(** [filter p s] keeps elements satisfying predicate [p]. *)
val filter : ('a -> bool) -> 'a stream -> 'a stream

(** [fold_left f init s] folds over the stream from left.
    Only works on finite prefix of infinite stream. *)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b stream -> int -> 'a

(** {1 Infinite Stream Generators} *)

(** [nats n] generates infinite stream of natural numbers starting from [n]. *)
val nats : int -> int stream

(** [from_step start step] generates arithmetic progression. *)
val from_step : int -> int -> int stream

(** [fibs] generates Fibonacci sequence (0, 1, 1, 2, 3, 5, ...). *)
val fibs : int stream

(** [sieve s] implements the Sieve of Eratosthenes for primes. *)
val sieve : int stream -> int stream

(** [primes] generates infinite stream of prime numbers. *)
val primes : int stream

(** {1 Stream Zipping} *)

(** [zip s1 s2] combines two streams element-wise into pairs. *)
val zip : 'a stream -> 'b stream -> ('a * 'b) stream

(** [zip_with f s1 s2] combines two streams using function [f]. *)
val zip_with : ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream

(** {1 Pretty Printing} *)

val string_of_stream : int -> ('a -> string) -> 'a stream -> string
