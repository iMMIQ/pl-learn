open Lesson05_cps

let () =
  Printf.printf "\n=== Lesson 05: Continuations and CPS ===\n\n";

  Printf.printf "1. Direct Style vs CPS:\n";
  Printf.printf "   Direct: (3 + 4) * 5 = 35\n";

  let direct = Mul (Add (Const 3, Const 4), Const 5) in
  let cps_e = cps_transform direct in
  Printf.printf "   CPS: %s\n" (string_of_cps_expr cps_e);
  Printf.printf "   Result: %d\n\n" (eval_cps (fun _ -> raise Not_found) cps_e);

  Printf.printf "2. CPS Transformation explained:\n";
  Printf.printf "   Original: let x = 10 in x + 5\n";

  let e = Let ("x", Const 10, Add (Var "x", Const 5)) in
  let cps_e = cps_transform e in
  Printf.printf "   CPS: %s\n\n" (string_of_cps_expr cps_e);

  Printf.printf "3. Why CPS?\n";
  Printf.printf "   - No call stack needed\n";
  Printf.printf "   - Control flow explicit\n";
  Printf.printf "   - Easy to implement exceptions, coroutines\n";
  Printf.printf "   - Used in compilers (LLVM, etc.)\n";

  Printf.printf "\n=== End of Examples ===\n"
