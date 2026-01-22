(** Mark-Sweep Garbage Collection *)

(** Mark-sweep is the simplest GC algorithm:

    1. Mark: Start from roots, mark all reachable objects
    2. Sweep: Free all unmarked objects

    Time: O(heap_size) per collection
    Space: No additional space needed
    Fragmentation: Can occur
*)

(** [mark_and_sweep h roots] performs GC on heap [h] with root set [roots].
    Returns the number of objects freed.
*)
val mark_and_sweep : Heap.heap -> Heap.obj list -> int

(** [mark h obj] marks [obj] and all objects reachable from it. *)
val mark : Heap.heap -> Heap.obj -> unit

(** [sweep h] frees all unmarked objects. *)
val sweep : Heap.heap -> int
