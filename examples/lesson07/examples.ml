open Lesson07.Secd
open Lesson07.Env_machine

let print_secd e =
  Printf.printf "  SECD: %s\n"
    (match run (init e) with
     | VInt n -> string_of_int n
     | VClosure _ -> "<closure>")

let print_env e =
  Printf.printf "  Env:  %s\n"
    (match eval e with
     | VInt n -> string_of_int n
     | VClos _ -> "<closure>")

let () =
  Printf.printf "\n=== Lesson 07: Abstract Machines ===\n\n";

  Printf.printf "1. Constants:\n";
  let e = Const 42 in
  print_secd e;
  print_env e;

  Printf.printf "\n2. Arithmetic:\n";
  let e = Add (Add (Const 1, Const 2), Const 3) in
  Printf.printf "  Expression: (1 + 2) + 3\n";
  print_secd e;
  print_env e;

  Printf.printf "\n3. Identity Function:\n";
  let e = App (Abs ("x", Var "x"), Const 5) in
  Printf.printf "  Expression: (λx. x) 5\n";
  print_secd e;
  print_env e;

  Printf.printf "\n4. Higher-Order Function:\n";
  let e = App (Abs ("f", App (Var "f", Const 3)),
               Abs ("x", Add (Var "x", Const 2))) in
  Printf.printf "  Expression: (λf. f 3) (λx. x + 2)\n";
  print_secd e;
  print_env e;

  Printf.printf "\n5. Let Binding:\n";
  let e = Let ("x", Add (Const 3, Const 4), Add (Var "x", Const 5)) in
  Printf.printf "  Expression: let x = 3 + 4 in x + 5\n";
  print_secd e;
  print_env e;

  Printf.printf "\n6. Machine Trace:\n";
  Printf.printf "  Tracing (λx. x) 5:\n";
  let e = App (Abs ("x", Var "x"), Const 5) in
  Printf.printf "%s\n" (trace e);

  Printf.printf "\n=== End of Examples ===\n"
