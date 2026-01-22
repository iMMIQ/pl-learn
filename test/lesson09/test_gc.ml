open Alcotest
module H = Lesson09.Heap
module M = Lesson09.Mark_sweep

open H
open M

(* Heap tests *)

let test_create_heap () =
  let h = create_heap () in
  check int "empty heap" 0 h.size

let test_alloc_object () =
  let h = create_heap () in
  let obj = alloc h 1 0 in
  check bool "alloc succeeds" true (obj <> None);
  (* Cleanup *)
  (match obj with
   | Some _o -> h.objects.(0) <- None; h.size <- 0
   | None -> ())

let test_alloc_full () =
  let h = create_heap () in
  (* Fill the heap *)
  let rec fill n =
    if n >= max_objects then ()
    else
      match alloc h 1 0 with
      | Some _ -> fill (n + 1)
      | None -> ()
  in
  fill 0;
  check bool "heap is full" true (is_full h)

(* Mark-sweep tests *)

let test_mark_sweep_basic () =
  let h = create_heap () in
  let obj1 = alloc h 1 0 in
  let _obj2 = alloc h 1 0 in
  let roots = match obj1 with Some o -> [o] | None -> [] in
  let freed = mark_and_sweep h roots in
  check bool "one object freed" true (freed >= 1)

let test_mark_sweep_all () =
  let h = create_heap () in
  let _obj1 = alloc h 1 0 in
  let _obj2 = alloc h 1 0 in
  let _obj3 = alloc h 1 0 in
  let roots = [] in
  let freed = mark_and_sweep h roots in
  check int "all objects freed" 3 freed

let test_mark_sweep_none () =
  let h = create_heap () in
  let obj1 = alloc h 1 0 in
  let obj2 = alloc h 1 0 in
  let roots = match obj1, obj2 with
    | Some o1, Some o2 -> [o1; o2]
    | _ -> []
  in
  let freed = mark_and_sweep h roots in
  check int "no objects freed" 0 freed

(* Performance comparison *)

let test_mark_sweep_time () =
  let h = create_heap () in
  (* Allocate many objects *)
  for _i = 0 to 19 do
    ignore (alloc h 1 0) |> ignore;
  done;
  (* Time a collection *)
  let roots = [] in
  let freed = mark_and_sweep h roots in
  check int "freed objects" 20 freed

let () =
  run "Lesson09: Garbage Collection" [
    ("Heap", [
      ("empty heap", `Quick, test_create_heap);
      ("alloc succeeds", `Quick, test_alloc_object);
      ("heap is full", `Quick, test_alloc_full);
    ]);
    ("Mark-Sweep", [
      ("one object freed", `Quick, test_mark_sweep_basic);
      ("all objects freed", `Quick, test_mark_sweep_all);
      ("no objects freed", `Quick, test_mark_sweep_none);
      ("freed objects", `Quick, test_mark_sweep_time);
    ]);
  ]
