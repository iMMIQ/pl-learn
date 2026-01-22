open Alcotest
module T = Lesson03.Typed

(* Basic types *)

let test_typeof_true _ctx =
  check (option string) "true:Bool"
    (Some "Bool")
    (try Some (T.string_of_typ (T.typecheck T.TmTrue)) with _ -> None)

let test_typeof_zero _ctx =
  check (option string) "0:Nat"
    (Some "Nat")
    (try Some (T.string_of_typ (T.typecheck T.TmZero)) with _ -> None)

let test_typeof_identity _ctx =
  let e = T.TmAbs ("x", T.TyNat, T.TmVar ("x", T.TyNat)) in
  check (option string) "λx:Nat. x : Nat → Nat"
    (Some "Nat → Nat")
    (try Some (T.string_of_typ (T.typecheck e)) with _ -> None)

let test_typeof_arrow _ctx =
  let e = T.TmAbs ("f", T.TyArrow (T.TyNat, T.TyNat),
                 T.TmVar ("f", T.TyArrow (T.TyNat, T.TyNat))) in
  check (option string) "λf:Nat→Nat. f"
    (Some "(Nat → Nat) → Nat → Nat")
    (try Some (T.string_of_typ (T.typecheck e)) with _ -> None)

let test_if_type _ctx =
  let e = T.TmIf (T.TmTrue, T.TmFalse, T.TmTrue) in
  check (option string) "if true then false else true : Bool"
    (Some "Bool")
    (try Some (T.string_of_typ (T.typecheck e)) with _ -> None)

let test_succ_type _ctx =
  let e = T.TSucc T.TmZero in
  check (option string) "succ 0 : Nat"
    (Some "Nat")
    (try Some (T.string_of_typ (T.typecheck e)) with _ -> None)

let test_iszero_type _ctx =
  let e = T.TmIsZero T.TmZero in
  check (option string) "iszero 0 : Bool"
    (Some "Bool")
    (try Some (T.string_of_typ (T.typecheck e)) with _ -> None)

(* Type errors *)

let test_error_app_mismatch _ctx =
  let e = T.TmApp (T.TmAbs ("x", T.TyNat, T.TmVar ("x", T.TyNat)), T.TmTrue) in
  check bool "app mismatch raises TypeError"
    (match (try Some (T.typecheck e) with T.TypeError _ -> None | _ -> Some T.TyBool) with
     | None -> true
     | _ -> false)
    true

let test_error_if_cond _ctx =
  let e = T.TmIf (T.TmZero, T.TmTrue, T.TmFalse) in
  check bool "if cond not bool raises TypeError"
    (match (try Some (T.typecheck e) with T.TypeError _ -> None | _ -> Some T.TyBool) with
     | None -> true
     | _ -> false)
    true

let test_error_branches_mismatch _ctx =
  let e = T.TmIf (T.TmTrue, T.TmTrue, T.TmZero) in
  check bool "branches mismatch raises TypeError"
    (match (try Some (T.typecheck e) with T.TypeError _ -> None | _ -> Some T.TyBool) with
     | None -> true
     | _ -> false)
    true

(* Evaluation *)

let test_eval_identity _ctx =
  let e = T.TmApp (T.TmAbs ("x", T.TyNat, T.TmVar ("x", T.TyNat)), T.TmZero) in
  check bool "eval (λx:Nat. x) 0" true
    (match T.eval e with T.TmZero -> true | _ -> false)

let test_eval_if_true _ctx =
  let e = T.TmIf (T.TmTrue, T.TmZero, T.TSucc T.TmZero) in
  check bool "eval if true then 0 else succ 0" true
    (match T.eval e with T.TmZero -> true | _ -> false)

let () =
  run "Lesson03: Simply Typed Lambda Calculus" [
    ("Type Checking", [
      test_case "true:Bool" `Quick test_typeof_true;
      test_case "0:Nat" `Quick test_typeof_zero;
      test_case "λx:Nat. x" `Quick test_typeof_identity;
      test_case "λf:Nat→Nat. f" `Quick test_typeof_arrow;
      test_case "if expression" `Quick test_if_type;
      test_case "succ 0" `Quick test_succ_type;
      test_case "iszero 0" `Quick test_iszero_type;
    ]);
    ("Type Errors", [
      test_case "app mismatch" `Quick test_error_app_mismatch;
      test_case "if cond not bool" `Quick test_error_if_cond;
      test_case "branches mismatch" `Quick test_error_branches_mismatch;
    ]);
    ("Evaluation", [
      test_case "eval identity" `Quick test_eval_identity;
      test_case "eval if true" `Quick test_eval_if_true;
    ]);
  ]
