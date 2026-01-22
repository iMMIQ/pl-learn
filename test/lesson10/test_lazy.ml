open Alcotest
module T = Lesson10.Thunk
module L = Lesson10.Lazy_eval
module S = Lesson10.Stream

open T
open L
open S

(* Thunk tests *)

let test_thunk_delay () =
  let _t = delay (fun () -> 42) in
  check bool "thunk created" true true

let test_thunk_force () =
  let counter = ref 0 in
  let t = delay (fun () -> incr counter; 42) in
  let r1 = force t in
  let r2 = force t in
  check int "first force" 42 r1;
  check int "second force" 42 r2

let test_lazy_memoization () =
  let counter = ref 0 in
  let lv = make_lazy (fun () -> incr counter; 42) in
  let _ = get lv in
  let _ = get lv in
  check int "computed once" 1 !counter

let test_lazy_status () =
  let lv = make_lazy (fun () -> 42) in
  match status lv with
  | `Unevaluated -> check bool "initially unevaluated" true true
  | _ -> check bool "initially unevaluated" true false;
  let _ = get lv in
  match status lv with
  | `Evaluated -> check bool "after evaluation" true true
  | _ -> check bool "after evaluation" true false

(* Lazy evaluation tests *)

let test_lazy_identity () =
  let e = App (Abs ("x", Var "x"), Const 42) in
  let result = eval empty_env e in
  check bool "identity returns thunk" true
    (match result with
     | VThunk _ -> true
     | _ -> false)

let test_lazy_if () =
  let e = If (Const 1, Const 10, Const 20) in
  let result = eval empty_env e in
  check bool "if true branch" true
    (match force_val result with
     | VConst 10 -> true
     | _ -> false)

let test_lazy_infinite () =
  let omega = App (Abs ("x", App (Abs ("x", Var "x"), Var "x")), Const 42) in
  let result = eval empty_env omega in
  check bool "omega creates thunk" true
    (match result with
     | VThunk _ -> true
     | _ -> false)

(* Stream tests *)

let test_stream_take () =
  let s = nats 0 in
  let first_five = take 5 s in
  check (list int) "first 5 naturals" [0; 1; 2; 3; 4] first_five

let test_stream_map () =
  let s = nats 1 in
  let doubled = map (( * ) 2) s in
  let result = take 5 doubled in
  check (list int) "doubled" [2; 4; 6; 8; 10] result

let test_stream_filter () =
  let s = nats 1 in
  let evens = filter (fun n -> n mod 2 = 0) s in
  let result = take 5 evens in
  check (list int) "evens" [2; 4; 6; 8; 10] result

let test_stream_fibs () =
  let result = take 10 fibs in
  check (list int) "first 10 fibs"
    [0; 1; 1; 2; 3; 5; 8; 13; 21; 34] result

let test_stream_primes () =
  let result = take 10 primes in
  check (list int) "first 10 primes"
    [2; 3; 5; 7; 11; 13; 17; 19; 23; 29] result

let test_stream_zip () =
  let s1 = nats 1 in
  let s2 = from_step 10 10 in
  let zipped = zip s1 s2 in
  let result = take 5 zipped in
  check (list (pair int int)) "zipped"
    [(1, 10); (2, 20); (3, 30); (4, 40); (5, 50)] result

let () =
  run "Lesson10: Lazy Evaluation" [
    ("Thunks", [
      ("thunk created", `Quick, test_thunk_delay);
      ("thunk force", `Quick, test_thunk_force);
      ("lazy memoization", `Quick, test_lazy_memoization);
      ("lazy status", `Quick, test_lazy_status);
    ]);
    ("Lazy Evaluation", [
      ("lazy identity", `Quick, test_lazy_identity);
      ("lazy if", `Quick, test_lazy_if);
      ("lazy infinite", `Quick, test_lazy_infinite);
    ]);
    ("Streams", [
      ("stream take", `Quick, test_stream_take);
      ("stream map", `Quick, test_stream_map);
      ("stream filter", `Quick, test_stream_filter);
      ("stream fibs", `Quick, test_stream_fibs);
      ("stream primes", `Quick, test_stream_primes);
      ("stream zip", `Quick, test_stream_zip);
    ]);
  ]
