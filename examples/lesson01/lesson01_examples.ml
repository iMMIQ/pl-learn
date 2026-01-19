open Lesson01.Arithmetic

let () =
  Printf.printf "\n=== Lesson 01: Arithmetic Expressions ===\n\n";

  Printf.printf "1. Constants:\n";
  let e = Const 42 in
  Printf.printf "   %s = %d\n\n" (string_of_expr e) (run e);

  Printf.printf "2. Arithmetic:\n";
  let e = Add (Mul (Const 3, Const 4), Const 5) in
  Printf.printf "   %s = %d\n\n" (string_of_expr e) (run e);

  Printf.printf "3. Let binding:\n";
  let e = Let ("x", Const 10, Add (Var "x", Const 5)) in
  Printf.printf "   %s = %d\n\n" (string_of_expr e) (run e);

  Printf.printf "4. Parsing:\n";
  let e = parse "let x = 5 in x * 2" in
  Printf.printf "   let x = 5 in x * 2 = %d\n" (run e);
  Printf.printf "\n=== End of Examples ===\n"
