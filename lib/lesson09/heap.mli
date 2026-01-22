(** Lesson 09: Garbage Collection - Heap Management *)

(** {1 What is Garbage Collection?}

    Garbage Collection (GC) automatically reclaims memory that is no longer
    in use. It solves the memory management problem:

    - Manual: malloc/free, new/delete (error-prone)
    - Automatic: GC allocates and collects (safe, convenient)

    The key question: When is memory "no longer in use"?

    Answer: Memory is live if it's reachable from the roots.
*)

(** {1 Heap Configuration} *)

(** [heap_size] is the size of our heap (in words) *)
val heap_size : int

(** [max_objects] is the maximum number of objects *)
val max_objects : int

(** {1 Object Representation} *)

(** Object header with mark bit and size *)
type header = {
  mutable marked : bool;       (* Mark bit for GC *)
  size : int;                  (* Size in words *)
  tag : int;                   (* Type tag *)
}

(** Value types in our language *)
type value =
  | VInt of int
  | VBool of bool
  | VClosure of string * value list (* λx. e with env *)
  | VUnit                        (* Unit value *)

and obj = {
  hdr : header;
  data : value array;
  mutable forward : obj option;  (* For copying collector *)
}

(** {1 Heap State} *)

type heap = {
  mutable objects : obj option array;
  mutable alloc_ptr : int;        (* Next allocation pointer *)
  mutable size : int;             (* Current allocated count *)
}

(** {1 Heap Operations} *)

(** [create_heap ()] creates a fresh heap *)
val create_heap : unit -> heap

(** [alloc h size tag] allocates a new object.
    Returns None if heap is full.
*)
val alloc : heap -> int -> int -> obj option

(** [is_full h] checks if heap has no free space *)
val is_full : heap -> bool

(** [clear_marks h] clears all mark bits in the heap *)
val clear_marks : heap -> unit

(** [get_roots stack globals] returns the root set.
    Roots are values reachable from stack and globals.
*)
val get_roots : value list -> value list array -> obj list

(** [value_refs v] returns objects referenced by a value *)
val value_refs : value -> obj list

(** {1 Pretty Printing} *)

val string_of_obj : obj -> string
val string_of_heap : heap -> string
val heap_stats : heap -> string
