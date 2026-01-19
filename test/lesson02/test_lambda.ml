open Alcotest
module L = Lesson02.Lambda

(* Free variables *)

let test_free_var _ctx =
  check (list string) "Var x" ["x"] (L.free_vars (L.Var "x"))

let test_free_abs _ctx =
  check (list string) "λx. x" [] (L.free_vars (L.Abs ("x", L.Var "x")))

let test_free_mixed _ctx =
  check (list string) "λx. x y" ["y"]
    (L.free_vars (L.Abs ("x", L.App (L.Var "x", L.Var "y"))))

(* Substitution *)

let test_subst_simple _ctx =
  let e = L.Var "x" in
  let r = L.subst "x" (L.Var "y") e in
  check string "x -> y" "y" (match r with L.Var s -> s | _ -> "fail")

let test_subst_bound _ctx =
  let e = L.Abs ("x", L.Var "x") in
  let r = L.subst "x" (L.Var "y") e in
  let result = match r with L.Abs ("x", L.Var "x") -> true | _ -> false in
  check bool "bound x unchanged" true result

let test_subst_capture _ctx =
  (* (λy. x)[x := y] should rename y to avoid capture *)
  let e = L.Abs ("y", L.Var "x") in
  let r = L.subst "x" (L.Var "y") e in
  let result = match r with L.Abs (y', L.Var "y") -> y' <> "y" | _ -> false in
  check bool "capture avoided" true result

(* Beta reduction *)

let test_beta_identity _ctx =
  let e = L.App (L.Abs ("x", L.Var "x"), L.Var "y") in
  let r = L.reduce_one L.Normal_order e in
  let result = match r with Some (L.Var "y") -> true | _ -> false in
  check bool "(λx.x) y -> y" true result

(* Church encodings *)

let test_church_true _ctx =
  let e = L.App (L.App (L.church_true, L.Var "a"), L.Var "b") in
  let r = L.normalize ~max_steps:10 e in
  let result = match r with Some (L.Var "a") -> true | _ -> false in
  check bool "true a b -> a" true result

let test_church_false _ctx =
  let e = L.App (L.App (L.church_false, L.Var "a"), L.Var "b") in
  let r = L.normalize ~max_steps:10 e in
  let result = match r with Some (L.Var "b") -> true | _ -> false in
  check bool "false a b -> b" true result

let test_church_if _ctx =
  let e = L.App (L.App (L.App (L.church_if, L.church_true), L.Var "t"), L.Var "f") in
  let r = L.normalize ~max_steps:20 e in
  let result = match r with Some (L.Var "t") -> true | _ -> false in
  check bool "if true t f -> t" true result

let test_church_num_0 _ctx =
  check (option int) "0" (Some 0) (L.church_to_int L.church_zero)

let test_church_num_3 _ctx =
  let n = L.int_to_church 3 in
  check (option int) "3" (Some 3) (L.church_to_int n)

let test_church_add _ctx =
  let sum = L.App (L.App (L.church_add, L.int_to_church 2), L.int_to_church 3) in
  check (option int) "2+3=5" (Some 5) (L.church_to_int sum)

let test_church_mul _ctx =
  let prod = L.App (L.App (L.church_mul, L.int_to_church 3), L.int_to_church 4) in
  check (option int) "3*4=12" (Some 12) (L.church_to_int prod)

(* Pretty printing *)

let test_pp_var _ctx =
  check string "Var x" "x" (L.string_of_expr (L.Var "x"))

let test_pp_abs _ctx =
  check string "λx. x" "λx. x" (L.string_of_expr (L.Abs ("x", L.Var "x")))

let test_pp_app _ctx =
  check string "f x" "f x" (L.string_of_expr (L.App (L.Var "f", L.Var "x")))

let () =
  run "Lesson02: Lambda Calculus" [
    ("Free Variables", [
      test_case "Var x" `Quick test_free_var;
      test_case "λx. x" `Quick test_free_abs;
      test_case "λx. x y" `Quick test_free_mixed;
    ]);
    ("Substitution", [
      test_case "x -> y" `Quick test_subst_simple;
      test_case "bound unchanged" `Quick test_subst_bound;
      test_case "capture avoided" `Quick test_subst_capture;
    ]);
    ("Beta Reduction", [
      test_case "(λx.x) y -> y" `Quick test_beta_identity;
    ]);
    ("Church Encodings", [
      test_case "true a b -> a" `Quick test_church_true;
      test_case "false a b -> b" `Quick test_church_false;
      test_case "if true t f -> t" `Quick test_church_if;
      test_case "0" `Quick test_church_num_0;
      test_case "3" `Quick test_church_num_3;
      test_case "2+3=5" `Quick test_church_add;
      test_case "3*4=12" `Quick test_church_mul;
    ]);
    ("Pretty Printing", [
      test_case "Var x" `Quick test_pp_var;
      test_case "λx. x" `Quick test_pp_abs;
      test_case "f x" `Quick test_pp_app;
    ]);
  ]
