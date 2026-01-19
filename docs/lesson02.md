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

succ = λn. λs. λz. s ((n s) z)
add  = λm. λn. λs. λz. (m s) ((n s) z)
```

## Try It

```ocaml
(* In REPL *)
open Lesson02.Lambda;;

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
