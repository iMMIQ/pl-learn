open Alcotest
module T = Lesson06.Types
module U = Lesson06.Unify
module I = Lesson06.Inference

open T
open U
open I

(* Substitution tests *)

let test_empty_subst _ctx =
  check (list (pair string string)) "empty_subst"
    [] (empty_subst |> List.map (fun (v,t) -> (v, string_of_ty t)))

let test_single_subst _ctx =
  let s = single "a" TyBool in
  check bool "single creates binding" true
    (List.mem ("a", TyBool) s)

let test_compose_subst _ctx =
  let s1 = single "a" TyBool in
  let s2 = single "b" TyInt in
  let s = compose s1 s2 in
  check int "compose length" 2 (List.length s)

let test_apply_subst_var _ctx =
  let s = single "a" TyBool in
  let t = apply_subst s (TyVar "a") in
  check bool "apply to variable" true
    (match t with TyBool -> true | _ -> false)

(* Unification tests *)

let test_unify_same_var _ctx =
  let s = unify (TyVar "a") (TyVar "a") in
  check bool "unify same var" true
    (s = empty_subst)

let test_unify_var_type _ctx =
  let s = unify (TyVar "a") TyBool in
  check bool "unify var with type" true
    (List.mem ("a", TyBool) s)

let test_unify_arrow _ctx =
  let s = unify (TyArrow (TyVar "a", TyBool))
                 (TyArrow (TyInt, TyBool)) in
  check bool "unify arrow types" true
    (try let t = apply_subst s (TyVar "a") in
         match t with TyInt -> true | _ -> false
     with _ -> false)

let test_unify_fail _ctx =
  let caught = try
    unify TyBool TyInt |> ignore;
    false
  with Unify_error _ -> true
  in
  check bool "unify bool/int fails" true caught

(* Type inference tests *)

let test_infer_const _ctx =
  let e = EConst 42 in
  let t = typeof e in
  check string "const 42 : Int" "Int" (string_of_ty t)

let test_infer_bool _ctx =
  let e = EBool true in
  let t = typeof e in
  check string "true : Bool" "Bool" (string_of_ty t)

let test_infer_identity _ctx =
  (* λx. x : 'a -> 'a *)
  let e = EAbs ("x", EVar "x") in
  let t = typeof e in
  check bool "identity is function" true
    (match t with TyArrow _ -> true | _ -> false)

let test_infer_apply _ctx =
  (* (λx. x) 5 : Int *)
  let e = EApp (EAbs ("x", EVar "x"), EConst 5) in
  let t = typeof e in
  check string "(λx.x) 5 : Int" "Int" (string_of_ty t)

let test_infer_let_polymorphism _ctx =
  (* let id = λx. x in (id 5, id true) *)
  let id = EAbs ("x", EVar "x") in
  let e = ELet ("id", id,
               EBinOp ("*", EApp (EVar "id", EConst 5),
                       EApp (EVar "id", EConst 3))) in
  let t = typeof e in
  check string "let polymorphic" "Int" (string_of_ty t)

let test_infer_if _ctx =
  let e = EIf (EBool true, EConst 1, EConst 0) in
  let t = typeof e in
  check string "if true then 1 else 0 : Int" "Int" (string_of_ty t)

let test_infer_binop _ctx =
  let e = EBinOp ("+", EConst 3, EConst 5) in
  let t = typeof e in
  check string "3 + 5 : Int" "Int" (string_of_ty t)

let test_infer_compare _ctx =
  let e = EBinOp ("<", EConst 3, EConst 5) in
  let t = typeof e in
  check string "3 < 5 : Bool" "Bool" (string_of_ty t)

let test_infer_composition _ctx =
  (* λf. λg. λx. f (g x) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c *)
  let f = EVar "f" in
  let g = EVar "g" in
  let x = EVar "x" in
  let e = EAbs ("f", EAbs ("g", EAbs ("x", EApp (f, EApp (g, x))))) in
  let t = typeof e in
  check bool "composition has 3 arrows" true
    (match t with
     | TyArrow (_, TyArrow (_, TyArrow _)) -> true
     | _ -> false)

let () =
  run "Lesson06: Type Inference" [
    ("Substitutions", [
      test_case "empty_subst" `Quick test_empty_subst;
      test_case "single_subst" `Quick test_single_subst;
      test_case "compose_subst" `Quick test_compose_subst;
      test_case "apply_subst_var" `Quick test_apply_subst_var;
    ]);
    ("Unification", [
      test_case "unify_same_var" `Quick test_unify_same_var;
      test_case "unify_var_type" `Quick test_unify_var_type;
      test_case "unify_arrow" `Quick test_unify_arrow;
      test_case "unify_fail" `Quick test_unify_fail;
    ]);
    ("Type Inference", [
      test_case "infer_const" `Quick test_infer_const;
      test_case "infer_bool" `Quick test_infer_bool;
      test_case "infer_identity" `Quick test_infer_identity;
      test_case "infer_apply" `Quick test_infer_apply;
      test_case "infer_let_polymorphism" `Quick test_infer_let_polymorphism;
      test_case "infer_if" `Quick test_infer_if;
      test_case "infer_binop" `Quick test_infer_binop;
      test_case "infer_compare" `Quick test_infer_compare;
      test_case "infer_composition" `Quick test_infer_composition;
    ]);
  ]
