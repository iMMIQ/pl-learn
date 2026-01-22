open Lesson08.Subtype
open Lesson08.Typed

let print_typeof e =
  try
    let t = typecheck e in
    Printf.printf "  %s : %s\n"
      (string_of_expr e)
      (string_of_ty t)
  with
  | SubtypeError msg -> Printf.printf "  Error: %s\n" msg

let () =
  Printf.printf "\n=== Lesson 08: Subtyping ===\n\n";

  Printf.printf "1. Basic Subtyping:\n";
  Printf.printf "  Bot ≤ Int ≤ Top: %b\n" (is_subtype TyBot TyInt && is_subtype TyInt TyTop);
  Printf.printf "  Bool ≤ Top: %b\n" (is_subtype TyBool TyTop);
  Printf.printf "  Int ≤ Bool: %b\n\n" (is_subtype TyInt TyBool);

  Printf.printf "2. Record Width Subtyping:\n";
  Printf.printf "  {x,y} ≤ {x}: %b\n"
    (is_subtype (TyRecord ["x", TyInt; "y", TyInt])
                (TyRecord ["x", TyInt]));
  Printf.printf "  {x} ≤ {x,y}: %b\n\n"
    (is_subtype (TyRecord ["x", TyInt])
                (TyRecord ["x", TyInt; "y", TyInt]));

  Printf.printf "3. Record Depth Subtyping:\n";
  Printf.printf "  {x:Int} ≤ {x:Top}: %b\n"
    (is_subtype (TyRecord ["x", TyInt])
                (TyRecord ["x", TyTop]));
  Printf.printf "  {x:Top} ≤ {x:Int}: %b\n\n"
    (is_subtype (TyRecord ["x", TyTop])
                (TyRecord ["x", TyInt]));

  Printf.printf "4. Function Variance:\n";
  Printf.printf "  Int→Int ≤ Int→Top: %b (covariant)\n"
    (is_subtype (TyArrow (TyInt, TyInt))
                (TyArrow (TyInt, TyTop)));
  Printf.printf "  Top→Int ≤ Int→Int: %b (contravariant)\n"
    (is_subtype (TyArrow (TyTop, TyInt))
                (TyArrow (TyInt, TyInt)));
  Printf.printf "  Int→Top ≤ Top→Int: %b\n\n"
    (is_subtype (TyArrow (TyInt, TyTop))
                (TyArrow (TyTop, TyInt)));

  Printf.printf "5. Join (Least Upper Bound):\n";
  Printf.printf "  Int ∨ Bool = %s\n"
    (string_of_ty (join TyInt TyBool));
  Printf.printf "  Int ∨ Top = %s\n"
    (string_of_ty (join TyInt TyTop));
  Printf.printf "  {x:Int} ∨ {y:Bool} = %s\n\n"
    (string_of_ty (join (TyRecord ["x", TyInt])
                       (TyRecord ["y", TyBool])));

  Printf.printf "6. Meet (Greatest Lower Bound):\n";
  Printf.printf "  Int ∧ Bool = %s\n"
    (string_of_ty (meet TyInt TyBool));
  Printf.printf "  Int ∧ Top = %s\n"
    (string_of_ty (meet TyInt TyTop));
  Printf.printf "  {x:Int,y:Bool} ∧ {x:Int} = %s\n\n"
    (string_of_ty (meet (TyRecord ["x", TyInt; "y", TyBool])
                        (TyRecord ["x", TyInt])));

  Printf.printf "7. Type Checking with Subtyping:\n";
  print_typeof (ERecord ["x", EConst 5; "y", EConst 3]);
  print_typeof (EProj (ERecord ["x", EConst 5; "y", EConst 3], "x"));

  Printf.printf "\n8. Practical Example:\n";
  Printf.printf "  Function expecting {x:Int}:\n";
  let f = EAbs ("r", TyRecord ["x", TyInt], EProj (EVar "r", "x")) in
  Printf.printf "    f = %s\n" (string_of_expr f);
  Printf.printf "    f : %s\n" (string_of_ty (typecheck f));
  Printf.printf "  Pass {x:Int, y:Int} (wider record):\n";
  let arg = ERecord ["x", EConst 5; "y", EConst 3] in
  Printf.printf "    arg = %s\n" (string_of_expr arg);
  Printf.printf "    arg : %s\n" (string_of_ty (typecheck arg));
  Printf.printf "  Result type: %s\n"
    (string_of_ty (typecheck (EApp (f, arg))));

  Printf.printf "\n=== End of Examples ===\n"
