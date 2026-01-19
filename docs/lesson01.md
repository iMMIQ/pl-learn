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
