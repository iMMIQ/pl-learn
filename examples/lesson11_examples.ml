open Lesson11.Adt
open Lesson11.Pattern
open Lesson11.Examples

let () =
  Printf.printf "\n=== Lesson 11: Pattern Matching and ADTs ===\n\n";

  Printf.printf "1. Algebra of Types:\n";
  Printf.printf "   Product: a × b (tuples, records)\n";
  Printf.printf "   Sum: a + b (variants, tagged unions)\n";
  Printf.printf "   Counting: |a + b| = |a| + |b|, |a × b| = |a| × |b|\n\n";

  Printf.printf "2. Option Type:\n";
  Printf.printf "   type 'a option = Some of 'a | None\n";
  Printf.printf "   Example: %s\n"
    (match E.Some 42 with
     | E.Some x -> "Some(" ^ string_of_int x ^ ")"
     | E.None -> "None");
  Printf.printf "   Example: %s\n\n"
    (match E.None with
     | E.Some x -> "Some(" ^ string_of_int x ^ ")"
     | E.None -> "None");

  Printf.printf "3. Result Type:\n";
  Printf.printf "   type ('e, 'a) result = Ok of 'a | Error of 'e\n";
  Printf.printf "   Example: %s\n\n"
    (match (E.Ok 42 : (string, int) E.result) with
     | E.Ok x -> "Ok(" ^ string_of_int x ^ ")"
     | E.Error e -> "Error(" ^ e ^ ")");

  Printf.printf "4. List Type:\n";
  Printf.printf "   type 'a list = Nil | Cons of 'a * 'a list\n";
  let lst = E.Cons (1, E.Cons (2, E.Cons (3, E.Nil))) in
  Printf.printf "   List: [1; 2; 3]\n";
  Printf.printf "   Map (x*2): %s\n"
    (match E.list_map (( * ) 2) lst with
     | E.Cons (2, E.Cons (4, E.Cons (6, E.Nil))) -> "[2; 4; 6]"
     | _ -> "...");
  Printf.printf "   Length: %d\n\n" (E.list_length lst);

  Printf.printf "5. Tree Type:\n";
  Printf.printf "   type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree\n";
  let tree = E.Node (1, E.Node (2, E.Leaf, E.Leaf), E.Node (3, E.Leaf, E.Leaf)) in
  Printf.printf "   Size: %d\n" (E.tree_size tree);
  Printf.printf "   Height: %d\n\n" (E.tree_height tree);

  Printf.printf "6. Pattern Matching Examples:\n";
  Printf.printf "   Wildcard (_): matches anything\n";
  Printf.printf "   Variable (x): matches and binds\n";
  Printf.printf "   Const (42): matches specific value\n";
  Printf.printf "   Tuple (p1, p2): destructures tuples\n";
  Printf.printf "   Variant (C(p)): destructures variants\n";
  Printf.printf "   Or (p1 | p2): matches either pattern\n";
  Printf.printf "   As (p as x): binds whole value\n\n";

  Printf.printf "7. Exhaustiveness Checking:\n";
  let bool_patterns = [
    PConst (VBool true);
    PConst (VBool false);
  ] in
  Printf.printf "   Bool patterns exhaustive: %b\n"
    (P.is_exhaustive bool_patterns P.TBool);

  let incomplete_bool = [PConst (VBool true)] in
  Printf.printf "   Incomplete bool exhaustive: %b\n"
    (P.is_exhaustive incomplete_bool P.TBool);
  Printf.printf "   Missing: %s\n"
    (match P.missing_patterns incomplete_bool P.TBool with
     | None -> "none"
     | Some ps ->
         String.concat ", " (List.map P.string_of_pattern ps));
  Printf.printf "\n";

  Printf.printf "8. Expression Trees:\n";
  let expr = E.Add (E.Lit 3, E.Mul (E.Lit 4, E.Lit 5)) in
  Printf.printf "   eval(3 + 4*5) = %d\n" (E.eval_expr expr);

  let expr_to_opt = E.Mul (E.Lit 1, E.Add (E.Lit 0, E.Lit 5)) in
  let optimized = E.optimize_expr expr_to_opt in
  Printf.printf "   optimize(1 * (0 + 5)) = %d\n" (E.eval_expr optimized);
  Printf.printf "\n";

  Printf.printf "9. Algebra of Types Examples:\n";
  Printf.printf "   |Bool| = 2 (true, false)\n";
  Printf.printf "   |Unit| = 1 (())\n";
  Printf.printf "   |Bool × Bool| = 4 ((true,true), (true,false), ...)\n";
  Printf.printf "   |Bool + Bool| = 4 (in practice less useful)\n";
  Printf.printf "   |Option Bool| = |1 + Bool| = 3 (None, Some true, Some false)\n\n";

  Printf.printf "10. Real-World ADTs:\n";
  Printf.printf "    - Rust: enum, Option, Result\n";
  Printf.printf "    - Haskell: data, Maybe, Either\n";
  Printf.printf "    - OCaml: type, option, result\n";
  Printf.printf "    - Swift: enum, Optional\n";
  Printf.printf "    - Scala: sealed trait, case class\n\n";

  Printf.printf "=== End of Examples ===\n"
