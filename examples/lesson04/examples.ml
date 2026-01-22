module S = Lesson04.Smallstep
module B = Lesson04.Bigstep

let () =
  Printf.printf "\n=== Lesson 04: Operational Semantics ===\n\n";

  Printf.printf "1. Small-Step Reduction:\n";
  Printf.printf "   (1 + 2) + 3:\n";
  let e = S.Add (S.Add (S.Const 1, S.Const 2), S.Const 3) in
  Printf.printf "   %s\n\n" (S.trace e);

  Printf.printf "2. Let Binding:\n";
  let e = S.Let ("x", S.Add (S.Const 3, S.Const 4), S.Mul (S.Var "x", S.Const 2)) in
  Printf.printf "   %s\n\n" (S.trace e);

  Printf.printf "3. Big-Step Derivation:\n";
  let e = B.Add (B.Mul (B.Const 2, B.Const 3), B.Const 4) in
  let (_, deriv) = B.eval_derivation (fun _ -> raise Not_found) e in
  Printf.printf "   %s\n\n" (B.string_of_derivation deriv);

  Printf.printf "4. Comparison:\n";
  Printf.printf "   Small-step: step-by-step computation\n";
  Printf.printf "   Big-step: direct value with derivation tree\n";

  Printf.printf "\n=== End of Examples ===\n"
