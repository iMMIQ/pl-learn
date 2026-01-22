(* {1 Comparable Signature} *)

[@@@warning "-32-69"]  (* Suppress unused-value-declaration and unused-field for module types *)

module type Comparable = sig
  type t
  val compare : t -> t -> int
  val eq : t -> t -> bool
end

(* {1 Hashable Signature} *)

module type Hashable = sig
  type t
  val hash : t -> int
  val eq : t -> t -> bool
end

(* {1 Monad Signature} *)

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

(* {1 Functor Module Types} *)

module type MakeSet = functor (E : Comparable) -> sig
  type elt = E.t
  type t
  val empty : t
  val add : elt -> t -> t
  val mem : elt -> t -> bool
  val union : t -> t -> t
  val inter : t -> t -> t
end

module type MakeHashTable = functor (K : Hashable) -> sig
  type key = K.t
  type 'a t
  val empty : unit -> 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a option
  val mem : key -> 'a t -> bool
end

(* {1 Binary Search Tree Functor} *)

module type BinarySearchTree = sig
  type elt
  type t

  val empty : t
  val is_empty : t -> bool
  val add : elt -> t -> t
  val mem : elt -> t -> bool
  val to_list : t -> elt list
end

module MakeBST (E : Comparable) : BinarySearchTree with type elt = E.t = struct
  type elt = E.t

  type t =
    | Empty
    | Node of elt * t * t

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false

  let rec add x = function
    | Empty -> Node (x, Empty, Empty)
    | Node (v, left, right) ->
        let c = E.compare x v in
        if c = 0 then Node (v, left, right)  (* Already present *)
        else if c < 0 then Node (v, add x left, right)
        else Node (v, left, add x right)

  let rec mem x = function
    | Empty -> false
    | Node (v, left, right) ->
        let c = E.compare x v in
        c = 0 || (c < 0 && mem x left) || mem x right

  let rec to_list = function
    | Empty -> []
    | Node (v, left, right) ->
        to_list left @ [v] @ to_list right
end

(* {1 Hash Table Functor} *)

module type HashTable = sig
  type key
  type 'a t

  val empty : unit -> 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a option
  val remove : key -> 'a t -> 'a t
  val keys : 'a t -> key list
end

module MakeHashTable (K : Hashable) : HashTable with type key = K.t = struct
  type key = K.t

  type 'a bucket =
    | EmptyBucket
    | ConsBucket of (key * 'a) * 'a bucket

  type 'a t = {
    size : int;  (* Reserved for future resizing *)
    buckets : 'a bucket array;
  }

  let hash_key k = (K.hash k) mod 10  (* Fixed size for simplicity *)

  let empty () : 'a t = { size = 10; buckets = Array.make 10 EmptyBucket }

  let rec bucket_find key = function
    | EmptyBucket -> None
    | ConsBucket ((k, v), rest) ->
        if K.eq k key then Some v else bucket_find key rest

  let rec bucket_add key value = function
    | EmptyBucket -> ConsBucket ((key, value), EmptyBucket)
    | ConsBucket ((k, v), rest) ->
        if K.eq k key then ConsBucket ((key, value), rest)
        else ConsBucket ((k, v), bucket_add key value rest)

  let add key value tbl =
    let idx = hash_key key in
    let new_buckets = Array.copy tbl.buckets in
    new_buckets.(idx) <- bucket_add key value tbl.buckets.(idx);
    { tbl with buckets = new_buckets }

  let find key tbl =
    let idx = hash_key key in
    bucket_find key tbl.buckets.(idx)

  let remove key tbl =
    let idx = hash_key key in
    let rec bucket_remove = function
      | EmptyBucket -> EmptyBucket
      | ConsBucket ((k, v), rest) ->
          if K.eq k key then rest
          else ConsBucket ((k, v), bucket_remove rest)
    in
    let new_buckets = Array.copy tbl.buckets in
    new_buckets.(idx) <- bucket_remove tbl.buckets.(idx);
    { tbl with buckets = new_buckets }

  let keys tbl =
    let rec bucket_keys = function
      | EmptyBucket -> []
      | ConsBucket ((k, _), rest) -> k :: bucket_keys rest
    in
    Array.fold_left (fun acc bucket ->
      acc @ bucket_keys bucket) [] tbl.buckets
end

(* {1 Concrete Instances} *)

module IntComparable : Comparable with type t = int = struct
  type t = int
  let compare = Stdlib.compare
  let eq x y = x = y
end

module StringComparable : Comparable with type t = string = struct
  type t = string
  let compare = Stdlib.compare
  let eq x y = x = y
end

module IntHashable : Hashable with type t = int = struct
  type t = int
  let hash x = x
  let eq x y = x = y
end

module IntSet = MakeBST (IntComparable)
module StringSet = MakeBST (StringComparable)

(* {1 Monad Examples} *)

module OptionMonad : Monad with type 'a t = 'a option = struct
  type 'a t = 'a option

  let return x = Some x

  let bind = function
    | None -> fun _ -> None
    | Some x -> fun f -> f x

  let map f = function
    | None -> None
    | Some x -> Some (f x)
end

module ListMonad : Monad with type 'a t = 'a list = struct
  type 'a t = 'a list

  let return x = [x]

  let bind lst f =
    List.flatten (List.map f lst)

  let map = List.map
end

(* {1 Monad Transformers} *)

module type MonadTrans = sig
  type 'a inner
  type 'a t

  val lift : 'a inner -> 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module OptionT (M : Monad) :
  MonadTrans with type 'a inner = 'a M.t
               and type 'a t = 'a option M.t = struct
  type 'a inner = 'a M.t
  type 'a t = 'a option M.t

  let lift x = M.map (fun v -> Some v) x

  let return x = M.return (Some x)

  let bind m f =
    M.bind m (function
      | None -> M.return None
      | Some x -> f x)
end
