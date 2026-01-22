(** Copying/Stop-and-Copy Garbage Collection *)

(** Stop-and-copy divides heap into two semi-spaces:

    1. Allocate in "from-space"
    2. When full, copy live objects to "to-space"
    3. Swap spaces

    Time: O(live_data) per collection
    Space: 2x needed (only half used)
    Fragmentation: None (compacting)
*)

(** Semi-space configuration *)
type semispace = {
  mutable objects : Heap.obj option array;
  mutable alloc_ptr : int;
}

type copying_heap = {
  mutable from_space : semispace;
  mutable to_space : semispace;
  mutable active_from : bool;  (* true = from_space is active *)
}

(** [create_copying_heap ()] creates a new copying heap *)
val create_copying_heap : unit -> copying_heap

(** [copying_collect h roots] copies live objects to to-space.
    Returns the number of objects copied.
*)
val copying_collect : copying_heap -> Heap.obj list -> int

(** [copy_obj h obj] copies [obj] to to-space if not already copied.
    Returns the new location.
*)
val copy_obj : copying_heap -> Heap.obj -> Heap.obj
