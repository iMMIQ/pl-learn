(** Exception and Maybe Monads *)

(** {1 Maybe Monad} *)

(** The Maybe monad represents computations that can fail *)
module Maybe : sig
  type 'a t =
    | Just of 'a
    | Nothing

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val from_option : 'a option -> 'a t
  val to_option : 'a t -> 'a option
end

(** {1 Exception Monad} *)

(** The Exception monad represents computations that can fail with an error *)
module Result : sig
  type ('e, 'a) t =
    | Ok of 'a
    | Error of 'e

  val return : 'a -> ('e, 'a) t
  val bind : ('e, 'a) t -> ('a -> ('e, 'b) t) -> ('e, 'b) t
  val map : ('a -> 'b) -> ('e, 'a) t -> ('e, 'b) t

  val throw : 'e -> ('e, 'a) t
  val catch : ('e, 'a) t -> ('e -> ('e, 'a) t) -> ('e, 'a) t

  (** [attempt f] catches exceptions from pure function [f] *)
  val attempt : (unit -> 'a) -> (exn, 'a) t

  (** [lift_option e o] lifts option into result *)
  val lift_option : 'e -> 'a option -> ('e, 'a) t

  (** [option_to_error e] converts option to result with error [e] *)
  val option_to_error : 'e -> 'a option -> ('e, 'a) t
end

(** {1 Validation Applicative} *)

(** Validation accumulates errors instead of short-circuiting *)
module Validation : sig
  type ('e, 'a) t =
    | Valid of 'a
    | Invalid of 'e list

  val return : 'a -> ('e, 'a) t
  val map : ('a -> 'b) -> ('e, 'a) t -> ('e, 'b) t
  val ap : ('e, 'a -> 'b) t -> ('e, 'a) t -> ('e, 'b) t

  (** [validate] combines multiple validations, accumulating errors *)
  val validate : ('e list -> 'e list) -> ('e, 'a) t list -> ('e, 'a list) t
end

(** {1 Compatibility aliases} *)

(** For backward compatibility with the original plan *)

type 'a maybe = 'a Maybe.t =
  | Just of 'a
  | Nothing

type ('e, 'a) result = ('e, 'a) Result.t =
  | Ok of 'a
  | Error of 'e

type ('e, 'a) validation = ('e, 'a) Validation.t =
  | Valid of 'a
  | Invalid of 'e list

[@@@warning "-32"]

val return : 'a -> 'a maybe
val bind : 'a maybe -> ('a -> 'b maybe) -> 'b maybe
val map : ('a -> 'b) -> 'a maybe -> 'b maybe
val from_option : 'a option -> 'a maybe
val to_option : 'a maybe -> 'a option

(* Result module compatibility *)
val throw : 'e -> ('e, 'a) result
val catch : ('e, 'a) result -> ('e -> ('e, 'a) result) -> ('e, 'a) result
val attempt : (unit -> 'a) -> (exn, 'a) result
val lift_option : 'e -> 'a option -> ('e, 'a) result
val option_to_error : 'e -> 'a option -> ('e, 'a) result

(* Validation module compatibility *)
val ap : ('e, 'a -> 'b) validation -> ('e, 'a) validation -> ('e, 'b) validation
val validate : ('e list -> 'e list) -> ('e, 'a) validation list -> ('e, 'a list) validation
val return_valid : 'a -> ('e, 'a) validation
val map_valid : ('a -> 'b) -> ('e, 'a) validation -> ('e, 'b) validation
