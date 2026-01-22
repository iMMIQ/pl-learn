open Lesson14__Ast
open Lesson14__Compiler
open Lesson14__Vm

let run_program name e =
  Printf.printf "\n=== %s ===\n" name;
  Printf.printf "Expression: %s\n" (string_of_expr e);
  let prog = compile e in
  let code = Lesson14__Bytecode.resolve_labels prog in
  Printf.printf "\nBytecode:\n%s\n" (Lesson14__Bytecode.disassemble code);
  let vm = create_vm code in
  let result = run_from_entry vm "main" in
  Printf.printf "\nResult: %s\n" (string_of_value result)

let () =
  Printf.printf "\n=== Lesson 14: Compiler Backend (Bytecode/VM) ===\n";

  run_program "Constant" (EInt 42);

  run_program "Arithmetic"
    (EPrim (Mul, EPrim (Add, EInt 3, EInt 4), EInt 5));
  (* 3 * 4 + 5 = 17 *)

  run_program "Let Binding"
    (ELet ("x", EInt 10,
           EPrim (Add, EVar "x", EInt 5)));
  (* let x = 10 in x + 5 = 15 *)

  run_program "Nested Let"
    (ELet ("x", EInt 5,
           ELet ("y", EInt 10,
                EPrim (Add, EVar "x", EVar "y"))));
  (* let x = 5 in let y = 10 in x + y = 15 *)

  run_program "Conditional"
    (EIf (EPrim (Lt, EInt 3, EInt 5),
          EInt 100,
          EInt 0));
  (* if 3 < 5 then 100 else 0 = 100 *)

  run_program "Sequence"
    (ESeq [EInt 1; EInt 2; EInt 3]);
  (* seq [1; 2; 3] = 3 *)

  Printf.printf "\n=== Summary ===\n";
  Printf.printf "1. AST → Bytecode Compiler\n";
  Printf.printf "2. Stack-based Virtual Machine\n";
  Printf.printf "3. Variable access and let bindings\n";
  Printf.printf "4. Complete compilation pipeline\n\n";
