open Alcotest
module S = Lesson08.Subtype
module T = Lesson08.Typed

open S
open T

(* Basic subtype tests *)

let test_subtype_refl () =
  check bool "Int ≤ Int" true (is_subtype TyInt TyInt)

let test_subtype_bot () =
  check bool "Bot ≤ Int" true (is_subtype TyBot TyInt)

let test_subtype_top () =
  check bool "Int ≤ Top" true (is_subtype TyInt TyTop)

let test_subtype_not () =
  check bool "Int ≤ Bool is false" false (is_subtype TyInt TyBool)

(* Record width subtyping *)

let test_record_width () =
  let t1 = TyRecord ["x", TyInt; "y", TyInt] in
  let t2 = TyRecord ["x", TyInt] in
  check bool "{x:Int, y:Int} ≤ {x:Int}" true (is_subtype t1 t2)

let test_record_width_not () =
  let t1 = TyRecord ["x", TyInt] in
  let t2 = TyRecord ["x", TyInt; "y", TyInt] in
  check bool "{x:Int} ≤ {x:Int, y:Int} is false" false (is_subtype t1 t2)

(* Record depth subtyping *)

let test_record_depth () =
  let t1 = TyRecord ["x", TyInt] in
  let t2 = TyRecord ["x", TyTop] in
  check bool "{x:Int} ≤ {x:Top}" true (is_subtype t1 t2)

(* Function variance *)

let test_function_covariant () =
  let t1 = TyArrow (TyInt, TyInt) in
  let t2 = TyArrow (TyInt, TyTop) in
  check bool "Int→Int ≤ Int→Top (covariant)" true (is_subtype t1 t2)

let test_function_contravariant () =
  let t1 = TyArrow (TyTop, TyInt) in
  let t2 = TyArrow (TyInt, TyInt) in
  check bool "Top→Int ≤ Int→Int (contravariant)" true (is_subtype t1 t2)

let test_function_both () =
  let t1 = TyArrow (TyTop, TyInt) in
  let t2 = TyArrow (TyInt, TyTop) in
  check bool "Top→Int ≤ Int→Top (both)" true (is_subtype t1 t2)

let test_function_fail () =
  let t1 = TyArrow (TyInt, TyInt) in
  let t2 = TyArrow (TyTop, TyInt) in
  check bool "Int→Int ≤ Top→Int is false" false (is_subtype t1 t2)

(* Join tests *)

let test_join_same () =
  let t = join TyInt TyInt in
  check bool "join(Int, Int) = Int" true (t = TyInt)

let test_join_diff_base () =
  let t = join TyInt TyBool in
  check bool "join(Int, Bool) = Top" true (t = TyTop)

let test_join_bot () =
  let t = join TyBot TyInt in
  check bool "join(Bot, Int) = Int" true (t = TyInt)

(* Meet tests *)

let test_meet_same () =
  let t = meet TyInt TyInt in
  check bool "meet(Int, Int) = Int" true (t = TyInt)

let test_meet_diff_base () =
  let t = meet TyInt TyBool in
  check bool "meet(Int, Bool) = Bot" true (t = TyBot)

let test_meet_top () =
  let t = meet TyTop TyInt in
  check bool "meet(Top, Int) = Int" true (t = TyInt)

(* Type checking tests *)

let test_typeof_const () =
  let e = EConst 5 in
  check string "typeof 5" "Int" (string_of_ty (typecheck e))

let test_typeof_abs () =
  let e = EAbs ("x", TyInt, EVar "x") in
  check string "typeof λx:Int. x" "Int → Int" (string_of_ty (typecheck e))

let test_typeof_app () =
  let e = EApp (EAbs ("x", TyInt, EVar "x"), EConst 5) in
  check string "typeof (λx:Int.x) 5" "Int" (string_of_ty (typecheck e))

let test_typeof_record () =
  let e = ERecord ["x", EConst 5; "y", EConst 3] in
  check string "typeof {x=5, y=3}"
    "{ x:Int, y:Int }" (string_of_ty (typecheck e))

let test_typeof_proj () =
  let e = EProj (ERecord ["x", EConst 5; "y", EConst 3], "x") in
  check string "typeof {x=5, y=3}.x" "Int" (string_of_ty (typecheck e))

let test_subtype_app () =
  (* {x:Int,y:Int} ≤ {x:Int}, so we can pass wider record *)
  let f = EAbs ("r", TyRecord ["x", TyInt], EProj (EVar "r", "x")) in
  let arg = ERecord ["x", EConst 5; "y", EConst 3] in
  let e = EApp (f, arg) in
  check string "pass wider record to function expecting narrower" "Int"
    (string_of_ty (typecheck e))

let () =
  run "Lesson08: Subtyping" [
    ("Basic Subtype", [
      ("Int ≤ Int", `Quick, test_subtype_refl);
      ("Bot ≤ Int", `Quick, test_subtype_bot);
      ("Int ≤ Top", `Quick, test_subtype_top);
      ("Int ≤ Bool is false", `Quick, test_subtype_not);
    ]);
    ("Record Subtyping", [
      ("{x:Int, y:Int} ≤ {x:Int}", `Quick, test_record_width);
      ("{x:Int} ≤ {x:Int, y:Int} is false", `Quick, test_record_width_not);
      ("{x:Int} ≤ {x:Top}", `Quick, test_record_depth);
    ]);
    ("Function Variance", [
      ("Int→Int ≤ Int→Top (covariant)", `Quick, test_function_covariant);
      ("Top→Int ≤ Int→Int (contravariant)", `Quick, test_function_contravariant);
      ("Top→Int ≤ Int→Top (both)", `Quick, test_function_both);
      ("Int→Int ≤ Top→Int is false", `Quick, test_function_fail);
    ]);
    ("Join/Meet", [
      ("join(Int, Int) = Int", `Quick, test_join_same);
      ("join(Int, Bool) = Top", `Quick, test_join_diff_base);
      ("join(Bot, Int) = Int", `Quick, test_join_bot);
      ("meet(Int, Int) = Int", `Quick, test_meet_same);
      ("meet(Int, Bool) = Bot", `Quick, test_meet_diff_base);
      ("meet(Top, Int) = Int", `Quick, test_meet_top);
    ]);
    ("Type Checking", [
      ("typeof 5", `Quick, test_typeof_const);
      ("typeof λx:Int. x", `Quick, test_typeof_abs);
      ("typeof (λx:Int.x) 5", `Quick, test_typeof_app);
      ("typeof {x=5, y=3}", `Quick, test_typeof_record);
      ("typeof {x=5, y=3}.x", `Quick, test_typeof_proj);
      ("pass wider record to function expecting narrower", `Quick, test_subtype_app);
    ]);
  ]
