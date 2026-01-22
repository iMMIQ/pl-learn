open Lesson03.Typed

let print_type e =
  try
    Printf.printf "  %s : %s\n"
      (string_of_expr e)
      (string_of_typ (typecheck e))
  with
  | TypeError msg -> Printf.printf "  Error: %s\n" msg

let () =
  Printf.printf "\n=== Lesson 03: Simply Typed λ-Calculus ===\n\n";

  Printf.printf "1. Base Types:\n";
  print_type TmTrue;
  print_type TmFalse;
  print_type TmZero;

  Printf.printf "\n2. Type Annotations:\n";
  print_type (TmAbs ("x", TyNat, TmVar ("x", TyNat)));
  print_type (TmAbs ("f", TyArrow (TyNat, TyBool),
                     TmApp (TmVar ("f", TyArrow (TyNat, TyBool)), TmZero)));

  Printf.printf "\n3. If Expression:\n";
  print_type (TmIf (TmTrue, TmFalse, TmTrue));

  Printf.printf "\n4. Arithmetic:\n";
  print_type (TSucc TmZero);
  print_type (TmIsZero TmZero);

  Printf.printf "\n5. Type Error - Mismatch:\n";
  print_type (TmApp (TmAbs ("x", TyNat, TmVar ("x", TyNat)), TmTrue));

  Printf.printf "\n=== End of Examples ===\n"
