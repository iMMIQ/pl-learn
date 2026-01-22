# Lesson 11: Pattern Matching and Algebraic Data Types

**Learning Objectives:**
- Understand product and sum types
- Implement recursive data types
- Compile patterns to decision trees
- Perform exhaustiveness checking
- Use ADTs effectively

## What are Algebraic Data Types?

**Algebraic Data Types (ADTs)** are composite types formed by:
- **Product types**: Combining values (AND)
- **Sum types**: Choosing between variants (OR)
- **Recursive types**: Types that reference themselves

### The Algebra of Types

```
Unit      = 1 value
Bool      = 2 values (true, false)
a × b     = |a| × |b| values (product)
a + b     = |a| + |b| values (sum)
a → b     = |b|^|a| values (functions)
```

Examples:
```
|Unit|           = 1
|Bool|           = 2
|Bool × Bool|    = 4
|Bool + Bool|    = 4 (but different structure)
|Option Bool|    = |Unit + Bool| = 1 + 2 = 3
```

## Product Types

Product types combine multiple values into one.

### Tuples

```ocaml
type ('a, 'b) tuple =
  { fst : 'a; snd : 'b }

(* Bool × Bool has 4 values *)
(* (true, true), (true, false), (false, true), (false, false) *)
```

### Records

```ocaml
type person = {
  name : string;
  age : int;
}
```

## Sum Types

Sum types represent alternatives - one of several choices.

### Option Type

```ocaml
type 'a option =
  | Some of 'a   (* A value *)
  | None         (* No value *)

(* Option Bool has 3 values: None, Some true, Some false *)
```

### Result Type

```ocaml
type ('e, 'a) result =
  | Ok of 'a      (* Success *)
  | Error of 'e   (* Failure *)
```

### Enumerations

```ocaml
type color =
  | Red
  | Green
  | Blue

(* 3 values *)
```

## Recursive Types

Types that refer to themselves enable data structures.

### Lists

```ocaml
type 'a list =
  | Nil                    (* Empty list *)
  | Cons of 'a * 'a list   (* Head :: Tail *)

(* A list is either empty OR a head paired with a list *)
```

### Trees

```ocaml
type 'a tree =
  | Leaf                              (* Empty tree *)
  | Node of 'a * 'a tree * 'a tree    (* Value, Left, Right *)
```

### Expression Trees

```ocaml
type expr =
  | Lit of int
  | Add of expr * expr
  | Mul of expr * expr

(* An expression is a literal OR operation on expressions *)
```

## Pattern Matching

Pattern matching destructures ADTs:

```ocaml
match value with
| PWildcard -> ...          (* _ matches anything *)
| PVar x -> ...             (* x binds the value *)
| PConst c -> ...           (* c matches specific value *)
| PTuple [p1; p2] -> ...    (* Destructure tuple *)
| PVariant (C, p) -> ...    (* Destructure variant *)
| POr (p1, p2) -> ...       (* Matches either *)
```

### Examples

```ocaml
(* Option *)
match opt with
| Some x -> "Got: " ^ string_of_int x
| None -> "Nothing"

(* List *)
match lst with
| Nil -> "Empty"
| Cons (x, xs) -> "Head: " ^ string_of_int x

(* Tree *)
match tree with
| Leaf -> 0
| Node (v, left, right) -> v + sum left + sum right

(* OR pattern *)
match n with
| 0 | 1 -> "small"
| n -> "large: " ^ string_of_int n

(* AS pattern *)
match lst with
| (x :: _) as xs -> "First is " ^ x
| [] -> "Empty"
```

## Pattern Compilation

Patterns are compiled to **decision trees** for efficiency:

```
match p with
| A x -> 1
| B -> 2
| C y -> 3

Compiles to:

switch p:
  case A:  return 1 (with x bound)
  case B:  return 2
  case C:  return 3 (with y bound)
```

### Decision Tree Type

```ocaml
type decision_tree =
  | DLeaf of bindings                (* Success! *)
  | DFail                            (* No match *)
  | DSwitch of var * cases * default (* Test and branch *)
```

## Exhaustiveness Checking

The compiler checks if patterns cover all cases:

```ocaml
(* OK: Covers true and false *)
match b with
| true -> 1
| false -> 0

(* Warning: Missing false case! *)
match b with
| true -> 1

(* OK: Wildcard covers everything *)
match b with
| true -> 1
| _ -> 0
```

### Checking Algorithm

For each type:
1. **Bool**: Need `true`, `false`, or wildcard
2. **Variant**: Need all constructors or wildcard
3. **Tuple**: Check each element type
4. **List**: Need `Nil` and `Cons` patterns

### Example Warning

```
Warning: Pattern matching is not exhaustive.
Here is an example of a value that is not matched:
false

match b with
| true -> 1
...
```

## Classic ADT Examples

### Maybe Monad (Option)

```ocaml
let map f = function
  | Some x -> Some (f x)
  | None -> None

let bind f = function
  | Some x -> f x
  | None -> None

(* Safe division *)
let safe_div x y =
  if y = 0 then None
  else Some (x / y)

let ( >>= ) = bind

(* Chaining operations *)
let compute x y =
  safe_div x y >>= fun q ->
  safe_div 10 q >>= fun r ->
  Some (r + 1)
```

### Expression Trees

```ocaml
type expr =
  | Lit of int
  | Add of expr * expr
  | Mul of expr * expr

let rec eval = function
  | Lit n -> n
  | Add (e1, e2) -> eval e1 + eval e2
  | Mul (e1, e2) -> eval e1 * eval e2

let rec optimize = function
  | Add (Lit 0, e) -> optimize e
  | Add (e, Lit 0) -> optimize e
  | Mul (Lit 0, _) -> Lit 0
  | Mul (_, Lit 0) -> Lit 0
  | Mul (Lit 1, e) -> optimize e
  | Mul (e, Lit 1) -> optimize e
  | Add (Lit n1, Lit n2) -> Lit (n1 + n2)
  | Mul (Lit n1, Lit n2) -> Lit (n1 * n2)
  | Add (e1, e2) -> Add (optimize e1, optimize e2)
  | Mul (e1, e2) -> Mul (optimize e1, optimize e2)
  | e -> e
```

## GADTs (Generalized Algebraic Data Types)

GADTs allow more precise typing:

```ocaml
type _ expr =
  | Lit : int -> int expr
  | Add : int expr -> int expr -> int expr
  | Bool : bool -> bool expr

(* The type of the expression is reflected in the type! *)
let eval : type a. a expr -> a = function
  | Lit n -> n
  | Add (e1, e2) -> eval e1 + eval e2
  | Bool b -> b
```

## ADTs in Real Languages

| Language | Syntax | Example |
|----------|--------|---------|
| OCaml | `type t = A \| B of int` | Native |
| Haskell | `data T = A \| B Int` | Native |
| Rust | `enum T { A, B(i32) }` | Native |
| Swift | `enum T { case A, case B(Int) }` | Native |
| Scala | `sealed trait T`; `case object A` | Native |

## Try It

```ocaml
open Examples;;

(* Option *)
let x = Some 42;;
match x with
| Some n -> "Got " ^ string_of_int n
| None -> "Nothing";;

(* List *)
let lst = Cons (1, Cons (2, Cons (3, Nil)));;
list_length lst;;  (* 3 *)

(* Tree *)
let tree = Node (1,
  Node (2, Leaf, Leaf),
  Node (3, Leaf, Leaf));;
tree_size tree;;  (* 3
tree_height tree;; (* 2 *)
```

## Exercises

1. Implement a binary search tree with insert and lookup
2. Add exception patterns to the pattern compiler
3. Implement redundancy checking (unreachable patterns)
4. Create a JSON representation using ADTs
5. Implement a typed interpreter using GADTs

## Further Reading

- *Algebraic Data Types*, Wikipedia
- [Pattern Matching](https://ocaml.org/manual/patternmatching.html) - OCaml Manual
- *Programming in Haskell*, Hutton - Chapters on ADTs
- [Rust Enums](https://doc.rust-lang.org/book/en/ch06-00-enums.html) - The Rust Book

## Real-World Connection

- **Compiler ASTs**: Abstract syntax trees are recursive ADTs
- **Protocol Buffers**: Enums represent message variants
- **Error Handling**: Result/Either types for explicit errors
- **State Machines**: ADTs model state transitions
- **AST Transforms**: Pattern matching enables clean tree rewriting

## Next Lesson

[Lesson 12: Modules and Functors](lesson12.md)
