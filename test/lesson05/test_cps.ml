open Alcotest
open Lesson05__Cps
open Lesson05__Control

(* CPS transformation tests *)

let test_cps_const _ctx =
  let e = Const 42 in
  let cps_e = cps_transform e in
  let r = eval_cps (fun _ -> raise Not_found) cps_e in
  check int "Const 42" 42 r

let test_cps_add _ctx =
  let e = Add (Const 3, Const 5) in
  let cps_e = cps_transform e in
  let r = eval_cps (fun _ -> raise Not_found) cps_e in
  check int "3 + 5" 8 r

let test_cps_mul _ctx =
  let e = Mul (Const 4, Const 6) in
  let cps_e = cps_transform e in
  let r = eval_cps (fun _ -> raise Not_found) cps_e in
  check int "4 * 6" 24 r

let test_cps_nested _ctx =
  let e = Add (Mul (Const 3, Const 4), Const 5) in
  let cps_e = cps_transform e in
  let r = eval_cps (fun _ -> raise Not_found) cps_e in
  check int "(3 * 4) + 5" 17 r

let test_cps_let _ctx =
  let e = Let ("x", Const 10, Add (Var "x", Const 5)) in
  let cps_e = cps_transform e in
  let r = eval_cps (fun _ -> raise Not_found) cps_e in
  check int "let x = 10 in x + 5" 15 r

let test_cps_complex _ctx =
  let e = Let ("x", Mul (Const 3, Const 4),
               Let ("y", Add (Var "x", Const 2),
                    Mul (Var "x", Var "y"))) in
  let cps_e = cps_transform e in
  let r = eval_cps (fun _ -> raise Not_found) cps_e in
  check int "complex let" 168 r

(* Control operator tests *)

let test_eval_const_ext _ctx =
  let e = EConst 42 in
  let r = eval_ext (fun _ -> raise Not_found) e in
  check bool "Const ext" true
    (match r with VInt 42 -> true | _ -> false)

let test_eval_add_ext _ctx =
  let e = EPrim ("+", [EConst 3; EConst 5], EHalt) in
  let r = eval_ext (fun _ -> raise Not_found) e in
  check bool "Add ext" true
    (match r with VInt 8 -> true | _ -> false)

let () =
  run "Lesson05: Continuations and CPS" [
    ("CPS Transformation", [
      test_case "const" `Quick test_cps_const;
      test_case "add" `Quick test_cps_add;
      test_case "mul" `Quick test_cps_mul;
      test_case "nested" `Quick test_cps_nested;
      test_case "let" `Quick test_cps_let;
      test_case "complex" `Quick test_cps_complex;
    ]);
    ("Control Operators", [
      test_case "const ext" `Quick test_eval_const_ext;
      test_case "add ext" `Quick test_eval_add_ext;
    ]);
  ]
