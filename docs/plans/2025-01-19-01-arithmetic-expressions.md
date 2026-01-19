# Phase 1: Arithmetic Expressions

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement arithmetic expressions with AST, evaluation, and parsing.

**Architecture:** Simple interpreter with recursive evaluation and hand-written parser.

**Tech Stack:** OCaml, core language only (no external dependencies beyond stdlib)

**Prerequisites:** Phase 0 - Project Setup completed

---

## Task 1: Define AST Type

**Files:**
- Create: `lib/lesson01/dune`
- Create: `lib/lesson01/arithmetic.ml`
- Create: `lib/lesson01/arithmetic.mli`

**Step 1: Create lib/lesson01/dune**

```ocaml
(library
 (name lesson01)
 (public_name pl_learn.lesson01))
```

**Step 2: Create lib/lesson01/arithmetic.mli**

```ocaml
(** Lesson 01: Arithmetic Expressions *)

(** {1 Abstract Syntax} *)

type expr =
  | Const of int                      (** Integer constant *)
  | Add of expr * expr                (** Addition *)
  | Mul of expr * expr                (** Multiplication *)
  | Var of string                     (** Variable reference *)
  | Let of string * expr * expr       (** Let binding *)

(** {1 Evaluation} *)

val eval : (string -> int) -> expr -> int
val run : expr -> int

(** {1 Pretty Printing} *)

val string_of_expr : expr -> string
val pp_expr : Format.formatter -> expr -> unit
```

**Step 3: Create lib/lesson01/arithmetic.ml**

```ocaml
type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr
  | Var of string
  | Let of string * expr * expr

let rec eval env = function
  | Const n -> n
  | Add (e1, e2) -> eval env e1 + eval env e2
  | Mul (e1, e2) -> eval env e1 * eval env e2
  | Var x ->
      (try env x
       with Not_found -> raise (Invalid_argument ("Unbound: " ^ x)))
  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      let env' y = if y = x then v1 else env y in
      eval env' e2

let run e = eval (fun _ -> raise Not_found) e

let rec string_of_expr = function
  | Const n -> string_of_int n
  | Add (e1, e2) -> "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
  | Mul (e1, e2) -> "(" ^ string_of_expr e1 ^ " * " ^ string_of_expr e2 ^ ")"
  | Var x -> x
  | Let (x, e1, e2) ->
      "let " ^ x ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2

let pp_expr fmt e =
  Format.pp_print_string fmt (string_of_expr e)
```

**Step 4: Build**

```bash
dune build
```

**Step 5: Commit**

```bash
git add lib/lesson01/
git commit -m "feat(lesson01): add AST and evaluation"
```

---

## Task 2: Write Tests

**Files:**
- Create: `test/lesson01/dune`
- Create: `test/lesson01/test_arithmetic.ml`

**Step 1: Create test/lesson01/dune**

```ocaml
(test
 (name test_arithmetic)
 (libraries lesson01 alcotest))
```

**Step 2: Create test/lesson01/test_arithmetic.ml**

```ocaml
open Alcotest
open Lesson01_arithmetic

let test_const () =
  check int "constant 42" 42 (run (Const 42))

let test_add () =
  check int "3 + 5 = 8" 8 (run (Add (Const 3, Const 5)))

let test_mul () =
  check int "4 * 6 = 24" 24 (run (Mul (Const 4, Const 6)))

let test_nested () =
  let e = Add (Mul (Const 3, Const 4), Const 5) in
  check int "(3 * 4) + 5 = 17" 17 (run e)

let test_let () =
  let e = Let ("x", Const 10, Add (Var "x", Const 5)) in
  check int "let x = 10 in x + 5" 15 (run e)

let test_nested_let () =
  let e = Let ("x", Const 5,
               Let ("y", Const 3,
                    Add (Var "x", Var "y"))) in
  check int "let x = 5 in let y = 3 in x + y" 8 (run e)

let () =
  run "Lesson01: Arithmetic Expressions" [
    ("Evaluation", [
      test_const;
      test_add;
      test_mul;
      test_nested;
      test_let;
      test_nested_let;
    ]);
  ]
```

**Step 3: Run tests**

```bash
dune runtest
```

Expected: All 6 tests pass

**Step 4: Commit**

```bash
git add test/
git commit -m "test(lesson01): add evaluation tests"
```

---

## Task 3: Add Parser

**Files:**
- Modify: `lib/lesson01/arithmetic.mli`
- Modify: `lib/lesson01/arithmetic.ml`

**Step 1: Update arithmetic.mli** - Add parser signature

```ocaml
(** {1 Parsing} *)

type token =
  | TINT of int
  | TPLUS | TMUL
  | TLPAREN | TRPAREN
  | TLET | TIN | TIDENT of string
  | TEOF

val parse : string -> expr
exception Syntax_error of string
```

**Step 2: Update arithmetic.ml** - Add parser implementation

Add to the file after `pp_expr`:

```ocaml
(* {1 Parsing} *)

type token =
  | TINT of int
  | TPLUS | TMUL
  | TLPAREN | TRPAREN
  | TLET | TIN | TIDENT of string
  | TEOF

exception Syntax_error of string

(* Simple lexer *)
let tokenize (s: string) : token list =
  let len = String.length s in
  let rec pos i =
    if i >= len then []
    else
      let c = s.[i] in
      match c with
      | ' ' | '\t' | '\n' | '\r' -> pos (i + 1)
      | '(' -> TLPAREN :: pos (i + 1)
      | ')' -> TRPAREN :: pos (i + 1)
      | '+' -> TPLUS :: pos (i + 1)
      | '*' -> TMUL :: pos (i + 1)
      | '0' .. '9' ->
          let j = ref i in
          while !j < len && s.[!j] >= '0' && s.[!j] <= '9' do incr j done;
          TINT (int_of_string (String.sub s i (!j - i))) :: pos !j
      | 'a' .. 'z' ->
          let j = ref i in
          while !j < len && s.[!j] >= 'a' && s.[!j] <= 'z' do incr j done;
          let id = String.sub s i (!j - i) in
          (match id with
           | "let" -> TLET | "in" -> TIN | _ -> TIDENT id) :: pos !j
      | _ -> raise (Syntax_error ("Unexpected char: " ^ String.make 1 c))
  in
  pos 0 @ [TEOF]

(* Parser state *)
type parser = { tokens: token array; mutable pos: int }

let make_parser tokens = { tokens = Array.of_list tokens; pos = 0 }

let peek p =
  if p.pos >= Array.length p.tokens then TEOF
  else p.tokens.(p.pos)

let consume p =
  let tok = peek p in
  p.pos <- p.pos + 1;
  tok

let rec parse_expr p =
  match peek p with
  | TEOF -> raise (Syntax_error "Unexpected end")
  | _ -> parse_additive p

and parse_additive p =
  let left = parse_multiplicative p in
  match peek p with
  | TPLUS -> consume p; Add (left, parse_additive p)
  | _ -> left

and parse_multiplicative p =
  let left = parse_primary p in
  match peek p with
  | TMUL -> consume p; Mul (left, parse_multiplicative p)
  | _ -> left

and parse_primary p =
  match peek p with
  | TINT n -> consume p; Const n
  | TIDENT x -> consume p; Var x
  | TLPAREN ->
      consume p;
      let e = parse_expr p in
      (match peek p with
       | TRPAREN -> consume p; e
       | _ -> raise (Syntax_error "Expected ')'"))
  | TLET ->
      consume p;
      (match peek p with
       | TIDENT x ->
           consume p;
           (match peek p with
            | TIDENT "=" ->
                consume p;  (* skip "=" *)
                let e1 = parse_expr p in
                (match peek p with
                 | TIN ->
                     consume p;
                     let e2 = parse_expr p in
                     Let (x, e1, e2)
                 | _ -> raise (Syntax_error "Expected 'in'"))
            | _ -> raise (Syntax_error "Expected '='"))
       | _ -> raise (Syntax_error "Expected identifier after 'let'"))
  | _ -> raise (Syntax_error "Unexpected token")

let parse s =
  let tokens = tokenize s in
  let p = make_parser tokens in
  let e = parse_expr p in
  match peek p with
  | TEOF -> e
  | _ -> raise (Syntax_error "Unexpected trailing input")
```

**Step 3: Fix lexer to handle "=" token**

Update the lexer pattern matching to handle "=":

```ocaml
      | '+' -> TPLUS :: pos (i + 1)
      | '*' -> TMUL :: pos (i + 1)
      | '=' -> TIDENT "=" :: pos (i + 1)  (* Add this line *)
```

**Step 4: Build**

```bash
dune build
```

**Step 5: Add parser tests**

Add to `test/lesson01/test_arithmetic.ml`:

```ocaml
let test_parse_const () =
  let e = Lesson01_arithmetic.parse "42" in
  check int "parse constant" 42 (run e)

let test_parse_add () =
  let e = Lesson01_arithmetic.parse "3 + 5" in
  check int "parse 3 + 5" 8 (run e)

let test_parse_mul_precedence () =
  let e = Lesson01_arithmetic.parse "3 + 4 * 5" in
  check int "precedence: 3 + 4*5 = 23" 23 (run e)

let test_parse_parens () =
  let e = Lesson01_arithmetic.parse "(3 + 4) * 5" in
  check int "parens: (3+4)*5 = 35" 35 (run e)

let test_parse_let () =
  let e = Lesson01_arithmetic.parse "let x = 10 in x + 5" in
  check int "parse let" 15 (run e)
```

Update the test list:

```ocaml
let () =
  run "Lesson01: Arithmetic Expressions" [
    ("Evaluation", [...]);
    ("Parsing", [
      test_parse_const;
      test_parse_add;
      test_parse_mul_precedence;
      test_parse_parens;
      test_parse_let;
    ]);
  ]
```

**Step 6: Run tests**

```bash
dune runtest
```

**Step 7: Commit**

```bash
git add -A
git commit -m "feat(lesson01): add parser with precedence"
```

---

## Task 4: Create Runnable Examples

**Files:**
- Create: `examples/lesson01_examples.ml`
- Modify: `examples/dune`
- Modify: `examples/main.ml`

**Step 1: Create examples/lesson01_examples.ml**

```ocaml
open Lesson01_arithmetic

let () =
  Printf.printf "\n=== Lesson 01: Arithmetic Expressions ===\n\n";

  Printf.printf "1. Constants:\n";
  let e = Const 42 in
  Printf.printf "   %s = %d\n\n" (string_of_expr e) (run e);

  Printf.printf "2. Arithmetic:\n";
  let e = Add (Mul (Const 3, Const 4), Const 5) in
  Printf.printf "   %s = %d\n\n" (string_of_expr e) (run e);

  Printf.printf "3. Let binding:\n";
  let e = Let ("x", Const 10, Add (Var "x", Const 5)) in
  Printf.printf "   %s = %d\n\n" (string_of_expr e) (run e);

  Printf.printf "4. Parsing:\n";
  let e = parse "let x = 5 in x * 2" in
  Printf.printf "   let x = 5 in x * 2 = %d\n" (run e);
  Printf.printf "\n=== End of Examples ===\n"
```

**Step 2: Update examples/dune**

```ocaml
(executable
 (name lesson01_examples)
 (public_name lesson01_examples)
 (libraries lesson01))
```

**Step 3: Update examples/main.ml**

```ocaml
let () =
  Printf.printf "PL Learn - Examples\n";
  Printf.printf "Run: dune exec lesson01_examples\n"
```

**Step 4: Run examples**

```bash
dune exec lesson01_examples
```

**Step 5: Commit**

```bash
git add examples/
git commit -m "feat(lesson01): add runnable examples"
```

---

## Task 5: Documentation

**Files:**
- Create: `docs/lesson01.md`
- Create: `docs/index.md`

**Step 1: Create docs/lesson01.md**

```markdown
# Lesson 01: Arithmetic Expressions

**Learning Objectives:**
- Understand Abstract Syntax Trees (AST)
- Implement expression evaluation
- Build a simple parser
- Learn about environments

## Abstract Syntax Tree

An AST represents code as a data structure:

```ocaml
type expr =
  | Const of int              (* 42 *)
  | Add of expr * expr        (* e1 + e2 *)
  | Mul of expr * expr        (* e1 * e2 *)
  | Var of string             (* x *)
  | Let of string * expr * expr (* let x = e1 in e2 *)
```

Visual: `3 + 4 * 5` → `Add(3, Mul(4, 5))`

## Evaluation

```ocaml
let rec eval env = function
  | Const n -> n
  | Add (e1, e2) -> eval env e1 + eval env e2
  | Var x -> env x
  | Let (x, e1, e2) ->
      let v = eval env e1 in
      eval (fun y -> if y = x then v else env y) e2
```

## Try It

```bash
# In the REPL
dune utop lib

# use "top.ml";;
let e = Lesson01_arithmetic.parse "let x = 5 in x * 2";;
Lesson01_arithmetic.run e;;
(* Result: 10 *)
```

## Exercises

1. Add subtraction operator
2. Add integer division with zero-check
3. Add comparison operators (=, <)

## Next Lesson

[Lesson 02: Lambda Calculus](lesson02.md)
```

**Step 2: Create docs/index.md**

```markdown
# PL Learn

Interactive Programming Language Theory course.

## Lessons

1. [Arithmetic Expressions](lesson01.md) - AST, evaluation, parsing
2. [Lambda Calculus](lesson02.md) - β-reduction, Church encodings
3. [Simply Typed λ-Calculus](lesson03.md) - Type checking
4. [Operational Semantics](lesson04.md) - Small-step semantics

## Getting Started

```bash
make        # Build
make test   # Run tests
make repl   # Start REPL
```
```

**Step 3: Commit**

```bash
git add docs/
git commit -m "docs: add lesson01 documentation"
```

---

## Summary

After Phase 1, you have:
- ✅ Arithmetic expression language
- ✅ AST with 5 constructs
- ✅ Big-step evaluator
- ✅ Parser with operator precedence
- ✅ Tests and examples
- ✅ Documentation

**Next:** Phase 2 - Lambda Calculus
