open Alcotest
module A = Lesson01.Arithmetic

let test_const _ctx =
  check int "constant 42" 42 (A.run (A.Const 42))

let test_add _ctx =
  check int "3 + 5 = 8" 8 (A.run (A.Add (A.Const 3, A.Const 5)))

let test_mul _ctx =
  check int "4 * 6 = 24" 24 (A.run (A.Mul (A.Const 4, A.Const 6)))

let test_nested _ctx =
  let e = A.Add (A.Mul (A.Const 3, A.Const 4), A.Const 5) in
  check int "(3 * 4) + 5 = 17" 17 (A.run e)

let test_let _ctx =
  let e = A.Let ("x", A.Const 10, A.Add (A.Var "x", A.Const 5)) in
  check int "let x = 10 in x + 5" 15 (A.run e)

let test_nested_let _ctx =
  let e = A.Let ("x", A.Const 5,
                 A.Let ("y", A.Const 3,
                        A.Add (A.Var "x", A.Var "y"))) in
  check int "let x = 5 in let y = 3 in x + y" 8 (A.run e)

let test_parse_const _ctx =
  let e = A.parse "42" in
  check int "parse constant" 42 (A.run e)

let test_parse_add _ctx =
  let e = A.parse "3 + 5" in
  check int "parse 3 + 5" 8 (A.run e)

let test_parse_mul_precedence _ctx =
  let e = A.parse "3 + 4 * 5" in
  check int "precedence: 3 + 4*5 = 23" 23 (A.run e)

let test_parse_parens _ctx =
  let e = A.parse "(3 + 4) * 5" in
  check int "parens: (3+4)*5 = 35" 35 (A.run e)

let test_parse_let _ctx =
  let e = A.parse "let x = 10 in x + 5" in
  check int "parse let" 15 (A.run e)

let () =
  run "Lesson01: Arithmetic Expressions" [
    ("Evaluation", [
      test_case "constant 42" `Quick test_const;
      test_case "addition" `Quick test_add;
      test_case "multiplication" `Quick test_mul;
      test_case "nested expression" `Quick test_nested;
      test_case "let binding" `Quick test_let;
      test_case "nested let" `Quick test_nested_let;
    ]);
    ("Parsing", [
      test_case "parse constant" `Quick test_parse_const;
      test_case "parse add" `Quick test_parse_add;
      test_case "parse mul precedence" `Quick test_parse_mul_precedence;
      test_case "parse parens" `Quick test_parse_parens;
      test_case "parse let" `Quick test_parse_let;
    ]);
  ]
