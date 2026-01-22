open Lesson09.Heap
open Lesson09.Mark_sweep

let () =
  Printf.printf "\n=== Lesson 09: Garbage Collection ===\n\n";

  Printf.printf "1. Heap Allocation:\n";
  let h = create_heap () in
  Printf.printf "  Created heap: %s\n" (heap_stats h);

  let obj1 = alloc h 3 0 in
  Printf.printf "  Allocated object 1\n";
  Printf.printf "  %s\n" (heap_stats h);

  let _obj2 = alloc h 2 0 in
  Printf.printf "  Allocated object 2\n";
  Printf.printf "  %s\n" (heap_stats h);

  Printf.printf "\n2. Mark-Sweep Collection:\n";
  let roots = match obj1 with Some o -> [o] | None -> [] in
  Printf.printf "  Roots: %d objects\n" (List.length roots);

  let before = h.size in
  let freed = mark_and_sweep h roots in
  Printf.printf "  Freed: %d objects\n" freed;
  Printf.printf "  Before: %d, After: %d\n" before h.size;

  Printf.printf "\n3. Memory Leak Simulation:\n";
  let h2 = create_heap () in

  (* Allocate objects, lose references *)
  let objs = ref [] in
  for _i = 0 to 10 do
    match alloc h2 1 0 with
    | Some o -> objs := o :: !objs
    | None -> ()
  done;

  Printf.printf "  Allocated 10 objects: %s\n" (heap_stats h2);

  (* Keep only 2 roots *)
  let roots = match !objs with
    | o1 :: o2 :: _ -> [o1; o2]
    | _ -> []
  in

  let freed = mark_and_sweep h2 roots in
  Printf.printf "  After GC (keeping 2 roots): %s\n" (heap_stats h2);
  Printf.printf "  Freed: %d objects\n" freed;

  Printf.printf "\n4. GC Algorithm Comparison:\n";
  Printf.printf "  Mark-Sweep:\n";
  Printf.printf "    - Time: O(heap_size)\n";
  Printf.printf "    - Space: 1x heap\n";
  Printf.printf "    - Fragmentation: Yes\n";
  Printf.printf "\n  Copying:\n";
  Printf.printf "    - Time: O(live_data)\n";
  Printf.printf "    - Space: 2x heap (only half used)\n";
  Printf.printf "    - Fragmentation: No (compact)\n";

  Printf.printf "\n=== End of Examples ===\n"
