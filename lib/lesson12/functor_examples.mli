(** Classic Functor Examples *)

(** {1 Comparable Signature} *)

module type Comparable = sig
  type t
  val compare : t -> t -> int
  val eq : t -> t -> bool
end

(** {1 Hashable Signature} *)

module type Hashable = sig
  type t
  val hash : t -> int
  val eq : t -> t -> bool
end

(** {1 Monad Signature} *)

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

(** {1 Functor Examples} *)

(** [MakeSet] creates a set module from a comparable element type *)
module type MakeSet = functor (E : Comparable) -> sig
  type elt = E.t
  type t
  val empty : t
  val add : elt -> t -> t
  val mem : elt -> t -> bool
  val union : t -> t -> t
  val inter : t -> t -> t
end

(** [MakeHashTable] creates a hash table from a hashable key type *)
module MakeHashTable : functor (K : Hashable) -> sig
  type key = K.t
  type 'a t
  val empty : unit -> 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a option
  val remove : key -> 'a t -> 'a t
  val keys : 'a t -> key list
end

(** {1 Instantiations} *)

module IntComparable : Comparable with type t = int

module StringComparable : Comparable with type t = string

module IntHashable : Hashable with type t = int

module IntSet : sig
  type elt = int
  type t
  val empty : t
  val is_empty : t -> bool
  val add : elt -> t -> t
  val mem : elt -> t -> bool
  val to_list : t -> elt list
end

module StringSet : sig
  type elt = string
  type t
  val empty : t
  val is_empty : t -> bool
  val add : elt -> t -> t
  val mem : elt -> t -> bool
  val to_list : t -> elt list
end

(** {1 Monad Examples} *)

module OptionMonad : Monad with type 'a t = 'a option

module ListMonad : Monad with type 'a t = 'a list

(** {1 Monad Transformers} *)

module type MonadTrans = sig
  type 'a inner
  type 'a t
  val lift : 'a inner -> 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module OptionT : functor (M : Monad) -> MonadTrans
  with type 'a inner = 'a M.t
   and type 'a t = 'a option M.t
