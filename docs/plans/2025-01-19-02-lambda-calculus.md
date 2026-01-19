# Phase 2: Lambda Calculus

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement untyped lambda calculus with beta reduction and Church encodings.

**Architecture:** Term-based reduction with capture-avoiding substitution.

**Tech Stack:** OCaml, core language

**Prerequisites:** Phase 1 - Arithmetic Expressions completed

---

## Task 1: Define Lambda Calculus AST

**Files:**
- Create: `lib/lesson02/dune`
- Create: `lib/lesson02/lambda.ml`
- Create: `lib/lesson02/lambda.mli`

**Step 1: Create lib/lesson02/dune**

```ocaml
(library
 (name lesson02)
 (public_name pl_learn.lesson02))
```

**Step 2: Create lib/lesson02/lambda.mli**

```ocaml
(** Lesson 02: Lambda Calculus *)

(** {1 Syntax} *)

type expr =
  | Var of string                      (* Variable: x, y, z *)
  | Abs of string * expr               (* Abstraction: λx. e *)
  | App of expr * expr                 (* Application: e1 e2 *)

(** {1 Free Variables} *)

val free_vars : expr -> string list
val is_closed : expr -> bool

(** {1 Substitution} *)

(** [subst x e body] computes body[x := e] with capture avoidance *)
val subst : string -> expr -> expr -> expr

(** {1 Beta Reduction} *)

type strategy =
  | Normal_order        (* Leftmost-outermost *)
  | Applicative_order   (* Leftmost-innermost *)

val reduce_one : strategy -> expr -> expr option
val normalize : ?strategy:strategy -> ?max_steps:int -> expr -> expr option

(** {1 Church Encodings} *)

val church_true : expr
val church_false : expr
val church_if : expr
val church_pair : expr
val church_fst : expr
val church_snd : expr
val church_zero : expr
val church_succ : expr
val church_add : expr
val church_mul : expr

val church_to_int : expr -> int option
val int_to_church : int -> expr

(** {1 Pretty Printing} *)

val string_of_expr : expr -> string
val pp_expr : Format.formatter -> expr -> unit
```

**Step 3: Create lib/lesson02/lambda.ml**

```ocaml
type expr =
  | Var of string
  | Abs of string * expr
  | App of expr * expr

type strategy =
  | Normal_order
  | Applicative_order

(* {1 Free Variables} *)

let rec free_vars = function
  | Var x -> [x]
  | Abs (x, e) -> List.filter ((<>) x) (free_vars e)
  | App (e1, e2) ->
      free_vars e1 @ free_vars e
      |> List.sort_uniq String.compare

let is_closed e = free_vars e = []

(* {1 Substitution with Capture Avoidance} *)

let fresh x used =
  let rec gen suffix =
    let candidate = x ^ "_" ^ string_of_int suffix in
    if List.mem candidate used then gen (succ suffix)
    else candidate
  in
  if List.mem x used then gen 1 else x

let rec subst x e = function
  | Var y as expr ->
      if y = x then e else expr
  | Abs (y, body) as expr ->
      if y = x then expr
      else if List.mem y (free_vars e) then
        let used = x :: free_vars body @ free_vars e in
        let y' = fresh y used in
        let body' = subst y (Var y') body in
        Abs (y', subst x e body')
      else
        Abs (y, subst x e body)
  | App (e1, e2) ->
      App (subst x e e1, subst x e e2)

(* {1 Beta Reduction} *)

let rec reduce_one strategy = function
  | Var _ | Abs _ -> None
  | App (Abs (x, body), arg) ->
      Some (subst x arg body)
  | App (e1, e2) ->
      (match reduce_one strategy e1 with
       | Some e1' -> Some (App (e1', e2))
       | None ->
           match reduce_one strategy e2 with
           | Some e2' -> Some (App (e1, e2'))
           | None -> None)

let normalize ?(strategy=Normal_order) ?(max_steps=10000) e =
  let rec step e count =
    if count >= max_steps then None
    else
      match reduce_one strategy e with
      | None -> Some e
      | Some e' -> step e' (count + 1)
  in
  step e 0

(* {1 Church Encodings} *)

let church_true = Abs ("p", Abs ("q", Var "p"))
let church_false = Abs ("p", Abs ("q", Var "q"))
let church_if = Abs ("b", Abs ("t", Abs ("f",
  App (App (Var "b", Var "t"), Var "f"))))

let church_pair =
  Abs ("f", Abs ("s", Abs ("b",
    App (App (Var "b", Var "f"), Var "s"))))
let church_fst =
  Abs ("p", App (Var "p", Abs ("x", Abs ("y", Var "x"))))
let church_snd =
  Abs ("p", App (Var "p", Abs ("x", Abs ("y", Var "y"))))

let church_zero = Abs ("s", Abs ("z", Var "z"))

let church_succ =
  Abs ("n", Abs ("s", Abs ("z",
    App (Var "s", App (App (Var "n", Var "s"), Var "z")))))

let church_add =
  Abs ("m", Abs ("n", Abs ("s", Abs ("z",
    App (App (Var "m", Var "s"), App (App (Var "n", Var "s"), Var "z"))))))

let church_mul =
  Abs ("m", Abs ("n", Abs ("f",
    App (Var "m", App (Var "n", Var "f")))))

let rec int_to_church n =
  if n <= 0 then church_zero
  else App (church_succ, int_to_church (n - 1))

let church_to_int e =
  match normalize ~max_steps:1000
        (App (App (e, Abs ("n", Abs ("x", App (Var "x", Var "n")))),
              Abs ("_", Var "_"))) with
  | None -> None
  | Some e' ->
      let rec count = function
        | App (Abs ("_", Var "_), _) -> 0
        | App (Abs (_, App (Var "x", Var "n")), rest) -> 1 + count rest
        | _ -> -1
      in
      try Some (count e') with _ -> None

(* {1 Pretty Printing} *)

let rec string_of_expr = function
  | Var x -> x
  | Abs (x, e) -> "λ" ^ x ^ ". " ^ string_of_expr e
  | App (e1, e2) ->
      let left = match e1 with
        | Abs _ -> "(" ^ string_of_expr e1 ^ ")"
        | App _ -> "(" ^ string_of_expr e1 ^ ")"
        | _ -> string_of_expr e1
      in
      let right = match e2 with
        | Var _ | Abs _ -> string_of_expr e2
        | _ -> "(" ^ string_of_expr e2 ^ ")"
      in
      left ^ " " ^ right

let pp_expr fmt e =
  Format.pp_print_string fmt (string_of_expr e)
```

**Step 4: Build**

```bash
dune build
```

**Step 5: Commit**

```bash
git add lib/lesson02/
git commit -m "feat(lesson02): add lambda calculus AST and operations"
```

---

## Task 2: Write Lambda Tests

**Files:**
- Create: `test/lesson02/dune`
- Create: `test/lesson02/test_lambda.ml`

**Step 1: Create test/lesson02/dune**

```ocaml
(test
 (name test_lambda)
 (libraries lesson02 alcotest))
```

**Step 2: Create test/lesson02/test_lambda.ml**

```ocaml
open Alcotest
open Lesson02_lambda

(* Free variables *)

let test_free_var () =
  check (list string) "Var x" ["x"] (free_vars (Var "x"))

let test_free_abs () =
  check (list string) "λx. x" [] (free_vars (Abs ("x", Var "x")))

let test_free_mixed () =
  check (list string) "λx. x y" ["y"]
    (free_vars (Abs ("x", App (Var "x", Var "y"))))

(* Substitution *)

let test_subst_simple () =
  let e = Var "x" in
  let r = subst "x" (Var "y") e in
  check string "x -> y" "y" (match r with Var s -> s | _ -> "fail")

let test_subst_bound () =
  let e = Abs ("x", Var "x") in
  let r = subst "x" (Var "y") e in
  check bool "bound x not changed"
    (match r with Abs ("x", Var "x") -> true | _ -> false)

let test_subst_capture () =
  (* (λy. x)[x := y] should rename y to avoid capture *)
  let e = Abs ("y", Var "x") in
  let r = subst "x" (Var "y") e in
  check bool "capture avoided"
    (match r with Abs (y', Var "y") -> y' <> "y" | _ -> false)

(* Beta reduction *)

let test_beta_identity () =
  let e = App (Abs ("x", Var "x"), Var "y") in
  let r = reduce_one Normal_order e in
  check bool "(λx.x) y → y"
    (match r with Some (Var "y") -> true | _ -> false)

(* Church encodings *)

let test_church_true () =
  let e = App (App (church_true, Var "a"), Var "b") in
  let r = normalize ~max_steps:10 e in
  check bool "true a b → a"
    (match r with Some (Var "a") -> true | _ -> false)

let test_church_false () =
  let e = App (App (church_false, Var "a"), Var "b") in
  let r = normalize ~max_steps:10 e in
  check bool "false a b → b"
    (match r with Some (Var "b") -> true | _ -> false)

let test_church_if () =
  let e = App (App (App (church_if, church_true), Var "t"), Var "f") in
  let r = normalize ~max_steps:20 e in
  check bool "if true t f → t"
    (match r with Some (Var "t") -> true | _ -> false)

let test_church_num_0 () =
  check (option int) "0" (Some 0) (church_to_int church_zero)

let test_church_num_3 () =
  let n = int_to_church 3 in
  check (option int) "3" (Some 3) (church_to_int n)

let test_church_add () =
  let sum = App (App (church_add, int_to_church 2), int_to_church 3) in
  check (option int) "2+3=5" (Some 5) (church_to_int sum)

let test_church_mul () =
  let prod = App (App (church_mul, int_to_church 3), int_to_church 4) in
  check (option int) "3*4=12" (Some 12) (church_to_int prod)

(* Pretty printing *)

let test_pp_var () =
  check string "Var x" "x" (string_of_expr (Var "x"))

let test_pp_abs () =
  check string "λx. x" "λx. x" (string_of_expr (Abs ("x", Var "x")))

let test_pp_app () =
  check string "f x" "f x" (string_of_expr (App (Var "f", Var "x")))

let () =
  run "Lesson02: Lambda Calculus" [
    ("Free Variables", [
      test_free_var;
      test_free_abs;
      test_free_mixed;
    ]);
    ("Substitution", [
      test_subst_simple;
      test_subst_bound;
      test_subst_capture;
    ]);
    ("Beta Reduction", [
      test_beta_identity;
    ]);
    ("Church Encodings", [
      test_church_true;
      test_church_false;
      test_church_if;
      test_church_num_0;
      test_church_num_3;
      test_church_add;
      test_church_mul;
    ]);
    ("Pretty Printing", [
      test_pp_var;
      test_pp_abs;
      test_pp_app;
    ]);
  ]
```

**Step 3: Run tests**

```bash
dune runtest
```

**Step 4: Commit**

```bash
git add test/lesson02/
git commit -m "test(lesson02): add lambda calculus tests"
```

---

## Task 3: Create Lambda Examples

**Files:**
- Create: `examples/lesson02_examples.ml`

**Step 1: Create examples/lesson02_examples.ml**

```ocaml
open Lesson02_lambda

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
```

**Step 2: Update examples/dune**

```ocaml
(executable
 (name lesson02_examples)
 (public_name lesson02_examples)
 (libraries lesson02))
```

**Step 3: Run examples**

```bash
dune exec lesson02_examples
```

**Step 4: Commit**

```bash
git add examples/
git commit -m "feat(lesson02): add lambda calculus examples"
```

---

## Task 4: Documentation

**Files:**
- Create: `docs/lesson02.md`

**Step 1: Create docs/lesson02.md**

```markdown
# Lesson 02: Lambda Calculus

**Learning Objectives:**
- Understand lambda calculus syntax
- Implement capture-avoiding substitution
- Learn beta reduction strategies
- Explore Church encodings

## Syntax

```ocaml
type expr =
  | Var of string      (* x, y, z *)
  | Abs of string * expr (* λx. e *)
  | App of expr * expr (* e1 e2 *)
```

## Examples

| Lambda Calculus | Meaning |
|----------------|---------|
| `λx. x` | Identity function |
| `λx. λy. x` | Constant function |
| `(λx. x) y` | Apply identity to y |

## Beta Reduction

```
(λx. body) arg  →  body[x := arg]
```

### Example

```
(λx. λy. x) (λz. z)
→ λy. (λx. λy. x)[x := λz. z]
→ λy. λz. z
```

## Church Encodings

Booleans:
```
true  = λp. λq. p
false = λp. λq. q
if    = λb. λt. λf. b t f
```

Numerals:
```
0 = λs. λz. z
1 = λs. λz. s z
2 = λs. λz. s (s z)

succ = λn. λs. λz. s (n s z)
add  = λm. λn. λs. λz. m s (n s z)
```

## Try It

```ocaml
(* In REPL *)
open Lesson02_lambda;;

let two = int_to_church 2;;
let three = int_to_church 3;;

church_to_int (App (App (church_add, two), three));;
(* Some 5 *)
```

## Exercises

1. Implement `church_and` and `church_or`
2. Implement `church_pred` (predecessor)
3. Implement Y combinator for recursion

## Next Lesson

[Lesson 03: Simply Typed λ-Calculus](lesson03.md)
```

**Step 2: Update docs/index.md**

Add to lessons list:
```markdown
2. [Lambda Calculus](lesson02.md) - β-reduction, Church encodings
```

**Step 3: Commit**

```bash
git add docs/
git commit -m "docs: add lesson02 lambda calculus documentation"
```

---

## Summary

After Phase 2, you have:
- ✅ Lambda calculus implementation
- ✅ Capture-avoiding substitution
- ✅ Beta reduction with strategies
- ✅ Church encodings (bool, pairs, numerals)
- ✅ Tests and examples
- ✅ Documentation

**Next:** Phase 3 - Simply Typed λ-Calculus
