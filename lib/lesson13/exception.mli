(** Exception and Maybe Monads *)

(** {1 Maybe Monad} *)

(** The Maybe monad represents computations that can fail *)
type 'a maybe

[@@@warning "-32"]

val return : 'a -> 'a maybe
val bind : 'a maybe -> ('a -> 'b maybe) -> 'b maybe
val map : ('a -> 'b) -> 'a maybe -> 'b maybe
val from_option : 'a option -> 'a maybe
val to_option : 'a maybe -> 'a option

(** {1 Exception Monad} *)

(** The Exception monad represents computations that can fail with an error *)
type ('e, 'a) result

val return : 'a -> ('e, 'a) result
val bind : ('e, 'a) result -> ('a -> ('e, 'b) result) -> ('e, 'b) result
val map : ('a -> 'b) -> ('e, 'a) result -> ('e, 'b) result

val throw : 'e -> ('e, 'a) result
val catch : ('e, 'a) result -> ('e -> ('e, 'a) result) -> ('e, 'a) result

(** {1 Monad Operations} *)

(** [attempt f] catches exceptions from pure function [f] *)
val attempt : (unit -> 'a) -> (exn, 'a) result

(** [lift_option e o] lifts option into result *)
val lift_option : 'e -> 'a option -> ('e, 'a) result

(** [option_to_error e] converts option to result with error [e] *)
val option_to_error : 'e -> 'a option -> ('e, 'a) result

(** {1 Validation Applicative} *)

(** Validation accumulates errors instead of short-circuiting *)
type ('e, 'a) validation

val return_valid : 'a -> ('e, 'a) validation
val map_valid : ('a -> 'b) -> ('e, 'a) validation -> ('e, 'b) validation
val ap : ('e, 'a -> 'b) validation -> ('e, 'a) validation -> ('e, 'b) validation

(** [validate] combines multiple validations, accumulating errors *)
val validate : ('e list -> 'e list) -> ('e, 'a) validation list -> ('e, 'a list) validation
