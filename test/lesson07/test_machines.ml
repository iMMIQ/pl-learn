open Alcotest
module S = Lesson07.Secd
module E = Lesson07.Env_machine

(* SECD machine tests *)

let test_secd_const _ctx =
  let e = S.Const 42 in
  let r = S.run (S.init e) in
  check bool "const 42" true
    (match r with S.VInt 42 -> true | _ -> false)

let test_secd_add _ctx =
  let e = S.Add (S.Const 3, S.Const 5) in
  let r = S.run (S.init e) in
  check bool "3+5=8" true
    (match r with S.VInt 8 -> true | _ -> false)

let test_secd_nested _ctx =
  let e = S.Add (S.Add (S.Const 1, S.Const 2), S.Const 3) in
  let r = S.run (S.init e) in
  check bool "(1+2)+3=6" true
    (match r with S.VInt 6 -> true | _ -> false)

let test_secd_abs _ctx =
  let e = S.Abs ("x", S.Var "x") in
  let r = S.run (S.init e) in
  check bool "λx.x returns closure" true
    (match r with S.VClosure _ -> true | _ -> false)

(* Environment machine tests *)

let test_env_const _ctx =
  let e = S.Const 42 in
  let r = E.eval e in
  check bool "env: const 42" true
    (match r with E.VInt 42 -> true | _ -> false)

let test_env_add _ctx =
  let e = S.Add (S.Const 3, S.Const 5) in
  let r = E.eval e in
  check bool "env: 3+5=8" true
    (match r with E.VInt 8 -> true | _ -> false)

let test_env_identity _ctx =
  (* (λx. x) 5 *)
  let e = S.App (S.Abs ("x", S.Var "x"), S.Const 5) in
  let r = E.eval e in
  check bool "env: (λx.x) 5 = 5" true
    (match r with E.VInt 5 -> true | _ -> false)

let test_env_apply _ctx =
  (* (λx. λy. x) 5 3 *)
  let e = S.App (S.App (S.Abs ("x", S.Abs ("y", S.Var "x")), S.Const 5), S.Const 3) in
  let r = E.eval e in
  check bool "env: const function returns 5" true
    (match r with E.VInt 5 -> true | _ -> false)

let test_env_let _ctx =
  let e = S.Let ("x", S.Const 10, S.Add (S.Var "x", S.Const 5)) in
  let r = E.eval e in
  check bool "env: let x=10 in x+5 = 15" true
    (match r with E.VInt 15 -> true | _ -> false)

let test_env_arith _ctx =
  (* (λf. f 3) (λx. x + 2) *)
  let f = S.Abs ("f", S.App (S.Var "f", S.Const 3)) in
  let g = S.Abs ("x", S.Add (S.Var "x", S.Const 2)) in
  let e = S.App (f, g) in
  let r = E.eval e in
  check bool "env: higher-order arithmetic" true
    (match r with E.VInt 5 -> true | _ -> false)

(* Comparison tests *)

let test_machines_agree_const _ctx =
  let e = S.Const 42 in
  let r1 = S.run (S.init e) in
  let r2 = E.eval e in
  let v1 = match r1 with S.VInt n -> n | _ -> -1 in
  let v2 = match r2 with E.VInt n -> n | _ -> -1 in
  check int "machines agree on const" v1 v2

let test_machines_agree_add _ctx =
  let e = S.Add (S.Const 3, S.Const 5) in
  let r1 = S.run (S.init e) in
  let r2 = E.eval e in
  let v1 = match r1 with S.VInt n -> n | _ -> -1 in
  let v2 = match r2 with E.VInt n -> n | _ -> -1 in
  check int "machines agree on add" v1 v2

let () =
  run "Lesson07: Abstract Machines" [
    ("SECD Machine", [
      test_case "const" `Quick test_secd_const;
      test_case "add" `Quick test_secd_add;
      test_case "nested" `Quick test_secd_nested;
      test_case "abs" `Quick test_secd_abs;
    ]);
    ("Environment Machine", [
      test_case "const" `Quick test_env_const;
      test_case "add" `Quick test_env_add;
      test_case "identity" `Quick test_env_identity;
      test_case "apply" `Quick test_env_apply;
      test_case "let" `Quick test_env_let;
      test_case "arith" `Quick test_env_arith;
    ]);
    ("Comparison", [
      test_case "agree_const" `Quick test_machines_agree_const;
      test_case "agree_add" `Quick test_machines_agree_add;
    ]);
  ]
