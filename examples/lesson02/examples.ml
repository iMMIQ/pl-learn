open Lesson02.Lambda

let () =
  Printf.printf "\n=== Lesson 02: Lambda Calculus ===\n\n";

  Printf.printf "1. Identity: λx. x\n";
  Printf.printf "   %s\n\n" (string_of_expr (Abs ("x", Var "x")));

  Printf.printf "2. Church Booleans:\n";
  Printf.printf "   true  = %s\n" (string_of_expr church_true);
  Printf.printf "   false = %s\n\n" (string_of_expr church_false);

  Printf.printf "3. Church Numerals:\n";
  Printf.printf "   0 = %s\n" (string_of_expr church_zero);
  Printf.printf "   3 = %s\n" (string_of_expr (int_to_church 3));

  Printf.printf "\n4. Arithmetic: 2 + 3 = ";
  (match church_to_int (App (App (church_add, int_to_church 2), int_to_church 3)) with
   | Some n -> Printf.printf "%d\n" n
   | None -> Printf.printf "?\n");

  Printf.printf "   3 * 4 = ";
  (match church_to_int (App (App (church_mul, int_to_church 3), int_to_church 4)) with
   | Some n -> Printf.printf "%d\n" n
   | None -> Printf.printf "?\n");

  Printf.printf "\n5. Beta Reduction:\n";
  Printf.printf "   (λx. x) y → ";
  (match normalize (App (Abs ("x", Var "x"), Var "y")) with
   | Some e -> Printf.printf "%s\n" (string_of_expr e)
   | None -> Printf.printf "(no normal form)\n");

  Printf.printf "\n=== End of Examples ===\n"
