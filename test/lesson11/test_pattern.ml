open Alcotest
module A = Lesson11.Adt
module P = Lesson11.Pattern
module E = Lesson11.Examples

open A
open P

(* Pattern matching tests *)

let test_wildcard_match () =
  let result = match_value PWildcard (VInt 42) in
  check bool "wildcard matches int" true (result <> None)

let test_var_bind () =
  let result = match_value (PVar "x") (VInt 42) in
  check bool "var binds" true (result = Some [("x", VInt 42)])

let test_const_match () =
  check bool "const matches" true (matches (PConst (VInt 42)) (VInt 42));
  check bool "const doesn't match" true
    (not (matches (PConst (VInt 42)) (VInt 41)))

let test_tuple_match () =
  let p = PTuple [PConst (VInt 1); PVar "x"] in
  let v = VTuple [VInt 1; VInt 42] in
  let result = match_value p v in
  check bool "tuple pattern matches" true (result <> None)

let test_variant_match () =
  let p = PVariant ("Some", Some (PVar "x")) in
  let v = VVariant ("Some", Some (VInt 42)) in
  check bool "variant matches" true (matches p v)

let test_or_pattern () =
  let p = POr (PConst (VInt 1), PConst (VInt 2)) in
  check bool "or matches 1" true (matches p (VInt 1));
  check bool "or matches 2" true (matches p (VInt 2));
  check bool "or doesn't match 3" true (not (matches p (VInt 3)))

let test_as_pattern () =
  let p = PAs ("whole", PTuple [PVar "x"; PVar "y"]) in
  let v = VTuple [VInt 1; VInt 2] in
  let result = match_value p v in
  check bool "as binds whole" true
    (match result with
     | Some [("whole", _); ("x", _); ("y", _)] -> true
     | _ -> false)

(* Exhaustiveness tests *)

let test_exhaustive_bool () =
  let ps = [PConst (VBool true); PConst (VBool false)] in
  check bool "bool patterns exhaustive" true
    (is_exhaustive ps P.TBool)

let test_exhaustive_bool_with_wildcard () =
  let ps = [PWildcard] in
  check bool "wildcard covers bool" true
    (is_exhaustive ps P.TBool)

let test_non_exhaustive_bool () =
  let ps = [PConst (VBool true)] in
  check bool "missing false" true
    (not (is_exhaustive ps P.TBool))

let test_exhaustive_variant () =
  let ty = P.TVariant [("A", None); ("B", None)] in
  let ps = [PVariant ("A", None); PVariant ("B", None)] in
  check bool "all variants covered" true
    (is_exhaustive ps ty)

(* ADT examples tests *)

let test_option_map () =
  check bool "Some map" true
    (match E.map_option ((+) 1) (E.Some 41) with
     | E.Some 42 -> true
     | _ -> false);
  check bool "None map" true
    (match E.map_option ((+) 1) E.None with
     | E.None -> true
     | _ -> false)

let test_option_bind () =
  let safe_div n d =
    if d = 0 then E.None
    else E.Some (n / d)
  in
  check bool "bind Some" true
    (match E.bind_option (fun x -> safe_div 10 x) (E.Some 2) with
     | E.Some 5 -> true
     | _ -> false);
  check bool "bind None" true
    (match E.bind_option (fun x -> safe_div 10 x) (E.Some 0) with
     | E.None -> true
     | _ -> false)

let test_list_map () =
  let lst = E.Cons (1, E.Cons (2, E.Cons (3, E.Nil))) in
  let mapped = E.list_map (( * ) 2) lst in
  check bool "list map" true
    (match mapped with
     | E.Cons (2, E.Cons (4, E.Cons (6, E.Nil))) -> true
     | _ -> false)

let test_list_length () =
  let lst = E.Cons (1, E.Cons (2, E.Cons (3, E.Nil))) in
  check int "list length" 3 (E.list_length lst)

let test_tree_size () =
  let t = E.Node (1, E.Node (2, E.Leaf, E.Leaf), E.Node (3, E.Leaf, E.Leaf)) in
  check int "tree size" 3 (E.tree_size t)

let test_tree_height () =
  let t = E.Node (1, E.Node (2, E.Leaf, E.Leaf), E.Leaf) in
  check int "tree height" 2 (E.tree_height t)

let test_expr_eval () =
  let e = E.Add (E.Lit 3, E.Mul (E.Lit 4, E.Lit 5)) in
  check int "expr eval" 23 (E.eval_expr e)

let test_expr_optimize () =
  let e = E.Mul (E.Lit 0, E.Add (E.Lit 1, E.Lit 2)) in
  check bool "optimize 0 * x" true
    (match E.optimize_expr e with
     | E.Lit 0 -> true
     | _ -> false)

let () =
  run "Lesson11: Pattern Matching and ADTs" [
    ("Pattern Matching", [
      ("wildcard matches", `Quick, test_wildcard_match);
      ("var binds", `Quick, test_var_bind);
      ("const matches", `Quick, test_const_match);
      ("tuple pattern matches", `Quick, test_tuple_match);
      ("variant matches", `Quick, test_variant_match);
      ("or pattern", `Quick, test_or_pattern);
      ("as pattern", `Quick, test_as_pattern);
    ]);
    ("Exhaustiveness", [
      ("bool patterns exhaustive", `Quick, test_exhaustive_bool);
      ("wildcard covers bool", `Quick, test_exhaustive_bool_with_wildcard);
      ("missing false", `Quick, test_non_exhaustive_bool);
      ("all variants covered", `Quick, test_exhaustive_variant);
    ]);
    ("Examples", [
      ("Some map", `Quick, test_option_map);
      ("bind Some", `Quick, test_option_bind);
      ("list map", `Quick, test_list_map);
      ("list length", `Quick, test_list_length);
      ("tree size", `Quick, test_tree_size);
      ("tree height", `Quick, test_tree_height);
      ("expr eval", `Quick, test_expr_eval);
      ("expr optimize", `Quick, test_expr_optimize);
    ]);
  ]
