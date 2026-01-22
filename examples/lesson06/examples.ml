module T = Lesson06.Types
module I = Lesson06.Inference
module U = Lesson06.Unify

open T
open I

let print_typeof e =
  try
    let t = typeof e in
    Printf.printf "  %s : %s\n"
      (I.string_of_expr e)
      (string_of_ty t)
  with
  | I.TypeError msg -> Printf.printf "  Error: %s\n" msg
  | U.Unify_error (msg, t1, t2) ->
      Printf.printf "  Unify error: %s (%s ≠ %s)\n" msg
        (string_of_ty t1) (string_of_ty t2)

let () =
  Printf.printf "\n=== Lesson 06: Type Inference ===\n\n";

  Printf.printf "1. Basic Types:\n";
  print_typeof (I.EConst 42);
  print_typeof (I.EBool true);

  Printf.printf "\n2. Type Inference:\n";
  print_typeof (I.EAbs ("x", I.EVar "x"));
  print_typeof (I.EAbs ("x", I.EConst 5));
  print_typeof (I.EAbs ("x", I.EAbs ("y", I.EApp (I.EVar "x", I.EVar "y"))));

  Printf.printf "\n3. Polymorphism via let:\n";
  let id = I.EAbs ("x", I.EVar "x") in
  print_typeof (I.ELet ("id", id,
    I.EBinOp ("+", I.EApp (I.EVar "id", I.EConst 5),
                   I.EApp (I.EVar "id", I.EConst 3))));

  Printf.printf "\n4. Higher-Order Functions:\n";
  let compose = I.EAbs ("f", I.EAbs ("g", I.EAbs ("x",
    I.EApp (I.EVar "f", I.EApp (I.EVar "g", I.EVar "x"))))) in
  print_typeof compose;

  Printf.printf "\n5. Conditionals:\n";
  print_typeof (I.EIf (I.EBool true, I.EConst 1, I.EConst 0));
  print_typeof (I.EIf (I.EVar "x", I.EConst 1, I.EConst 0));

  Printf.printf "\n6. Type Errors:\n";
  print_typeof (I.EApp (I.EConst 5, I.EConst 3));  (* Applying Int to Int *)
  print_typeof (I.EIf (I.EConst 1, I.EBool true, I.EBool false));  (* Non-bool condition *)

  Printf.printf "\n=== End of Examples ===\n"
