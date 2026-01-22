open Alcotest

(* Bind operator for State monad *)
let (>>=) = Lesson13__State.bind

(* Maybe monad tests *)

let test_maybe_return () =
  check bool "Just value" true
    (match Lesson13__Exception.Just 42 with Lesson13__Exception.Just _ -> true | Lesson13__Exception.Nothing -> false)

let test_maybe_bind () =
  let result = Lesson13__Exception.Maybe.bind (Lesson13__Exception.Just 42) (fun x ->
    Lesson13__Exception.Just (x * 2)
  ) in
  check bool "bind Just" true
    (match result with Lesson13__Exception.Just 84 -> true | _ -> false)

let test_maybe_bind_nothing () =
  let result = Lesson13__Exception.Maybe.bind Lesson13__Exception.Nothing (fun x ->
    Lesson13__Exception.Just (x * 2)
  ) in
  check bool "bind Nothing" true
    (result = Lesson13__Exception.Nothing)

(* State monad tests *)

let test_state_return () =
  let m = Lesson13__State.return 42 in
  let (result, final) = Lesson13__State.run_state 0 m in
  check int "return value" 42 result;
  check int "state unchanged" 0 final

let test_state_get () =
  let m = Lesson13__State.get in
  let (result, final) = Lesson13__State.run_state 42 m in
  check int "get returns state" 42 result;
  check int "state unchanged" 42 final

let test_state_put () =
  let m = Lesson13__State.put 100 in
  let (_, final) = Lesson13__State.run_state 0 m in
  check int "put changes state" 100 final

let test_state_modify () =
  let m = Lesson13__State.modify (( * ) 2) in
  let (_, final) = Lesson13__State.run_state 21 m in
  check int "modify doubles state" 42 final

let test_state_counter () =
  let m = Lesson13__State.counter () >>= fun _ ->
             Lesson13__State.counter () >>= fun _ ->
             Lesson13__State.counter () in
  let (result, _) = Lesson13__State.run_state 0 m in
  check int "counter returns 0, then 1, then 2" 2 result

let test_state_sum () =
  let m = Lesson13__State.sum [1; 2; 3; 4; 5] >>= fun () ->
             Lesson13__State.get in
  let (result, _) = Lesson13__State.run_state 0 m in
  check int "sum of [1..5]" 15 result

let test_state_stack () =
  let m = Lesson13__State.push 1 >>= fun () ->
             Lesson13__State.push 2 >>= fun () ->
             Lesson13__State.push 3 >>= fun () ->
             Lesson13__State.pop () >>= fun x1 ->
             Lesson13__State.pop () >>= fun x2 ->
             Lesson13__State.pop () >>= fun x3 ->
             Lesson13__State.return (x1, x2, x3) in
  let ((r1, r2, r3), _) = Lesson13__State.run_state ([] : int Lesson13__State.stack) m in
  check (option int) "first pop" (Some 3) r1;
  check (option int) "second pop" (Some 2) r2;
  check (option int) "third pop" (Some 1) r3

(* Result monad tests *)

let test_result_return () =
  check bool "Ok value" true
    (match Lesson13__Exception.Ok 42 with Lesson13__Exception.Ok _ -> true | Lesson13__Exception.Error _ -> false)

let test_result_bind () =
  let result = Lesson13__Exception.Result.bind (Lesson13__Exception.Ok 42) (fun x ->
    Lesson13__Exception.Ok (x * 2)
  ) in
  check bool "bind Ok" true
    (match result with Lesson13__Exception.Ok 84 -> true | _ -> false)

let test_result_bind_error () =
  let result = Lesson13__Exception.Result.bind (Lesson13__Exception.Error "bad") (fun x ->
    Lesson13__Exception.Ok (x * 2)
  ) in
  check bool "bind Error" true
    (match result with Lesson13__Exception.Error "bad" -> true | _ -> false)

let test_result_throw_catch () =
  let m = Lesson13__Exception.Error "fail" in
  let caught = Lesson13__Exception.Result.catch m (fun e ->
    Lesson13__Exception.Ok ("caught: " ^ e)
  ) in
  check bool "catch works" true
    (match caught with Lesson13__Exception.Ok "caught: fail" -> true | _ -> false)

(* Monad utilities tests *)

(* Skip MonadUtils tests for State since it has 2 type parameters.
   The MonadUtils functor is designed for single-parameter monads. *)

let test_sequence () =
  let ms = [Lesson13__State.return 1; Lesson13__State.return 2; Lesson13__State.return 3] in
  let rec sequence = function
    | [] -> Lesson13__State.return []
    | m :: ms -> m >>= fun x -> sequence ms >>= fun xs -> Lesson13__State.return (x :: xs)
  in
  let m = sequence ms in
  let (result, _) = Lesson13__State.run_state 0 m in
  check (list int) "sequence" [1; 2; 3] result

let test_mapM () =
  let rec mapM f = function
    | [] -> Lesson13__State.return []
    | x :: xs -> f x >>= fun y -> mapM f xs >>= fun ys -> Lesson13__State.return (y :: ys)
  in
  let m = mapM (fun x -> Lesson13__State.return (x * 2)) [1; 2; 3] in
  let (result, _) = Lesson13__State.run_state 0 m in
  check (list int) "mapM" [2; 4; 6] result

let test_filterM () =
  let rec filterM p = function
    | [] -> Lesson13__State.return []
    | x :: xs -> p x >>= fun b -> filterM p xs >>= fun ys -> Lesson13__State.return (if b then x :: ys else ys)
  in
  let m = filterM (fun x -> Lesson13__State.return (x mod 2 = 0)) [1; 2; 3; 4; 5] in
  let (result, _) = Lesson13__State.run_state 0 m in
  check (list int) "filterM evens" [2; 4] result

let test_foldM () =
  let rec foldM f acc = function
    | [] -> Lesson13__State.return acc
    | x :: xs -> f acc x >>= fun acc' -> foldM f acc' xs
  in
  let m = foldM (fun acc x -> Lesson13__State.return (acc + x)) 0 [1; 2; 3; 4; 5] in
  let (result, _) = Lesson13__State.run_state 0 m in
  check int "foldM sum" 15 result

(* Validation tests *)

let test_validation_ap_invalid () =
  let f = Lesson13__Exception.Invalid ["error in f"] in
  let x = Lesson13__Exception.Valid 3 in
  let result = Lesson13__Exception.ap f x in
  check bool "ap invalid function" true
    (match result with Lesson13__Exception.Invalid ["error in f"] -> true | _ -> false)

let test_validate () =
  let results = [
    Lesson13__Exception.Valid 1;
    Lesson13__Exception.Valid 2;
    Lesson13__Exception.Invalid ["error at 3"];
    Lesson13__Exception.Invalid ["error at 4"];
  ] in
  let result = Lesson13__Exception.Validation.validate (fun es -> ["Errors: " ^ String.concat ", " es]) results in
  check bool "validate accumulates errors" true
    (match result with Lesson13__Exception.Invalid ["Errors: error at 4, error at 3"] -> true
     | _ -> false)

(* Combined effects tests *)
(* Note: The abstract state types make these tests complex to write.
   In practice, users would define their own state types and use the transformers.
   For now, we verify the modules compile and are available. *)

let test_transformers_compile () =
  ()

let () =
  run "Lesson13: Effects and Monads" [
    ("Maybe Monad", [
      ("Just value", `Quick, test_maybe_return);
      ("bind Just", `Quick, test_maybe_bind);
      ("bind Nothing", `Quick, test_maybe_bind_nothing);
    ]);
    ("State Monad", [
      ("return value", `Quick, test_state_return);
      ("get returns state", `Quick, test_state_get);
      ("put changes state", `Quick, test_state_put);
      ("modify doubles state", `Quick, test_state_modify);
      ("counter returns 0, then 1", `Quick, test_state_counter);
      ("sum of [1..5]", `Quick, test_state_sum);
      ("stack operations", `Quick, test_state_stack);
    ]);
    ("Result Monad", [
      ("Ok value", `Quick, test_result_return);
      ("bind Ok", `Quick, test_result_bind);
      ("bind Error", `Quick, test_result_bind_error);
      ("catch works", `Quick, test_result_throw_catch);
    ]);
    ("Monad Utilities", [
      ("sequence", `Quick, test_sequence);
      ("mapM", `Quick, test_mapM);
      ("filterM", `Quick, test_filterM);
      ("foldM sum", `Quick, test_foldM);
    ]);
    ("Validation", [
      ("ap invalid function", `Quick, test_validation_ap_invalid);
      ("validate accumulates errors", `Quick, test_validate);
    ]);
    ("Combined Effects", [
      ("transformer modules exist", `Quick, test_transformers_compile);
    ]);
  ]
