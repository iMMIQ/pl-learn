# Lesson 12: Modules and Functors

**Learning Objectives:**
- Understand module signatures (interfaces)
- Understand module structures (implementations)
- Implement functors (parametrized modules)
- Use type abstraction effectively
- Build reusable components with functors

## What are Modules?

**Modules** are collections of types, values, and submodules that provide:
- **Namespace management**: Organize code and avoid naming conflicts
- **Information hiding**: Hide implementation details behind interfaces
- **Code reuse**: Functors allow parametrization over modules

## Module Signatures

A **signature** defines a module's interface:

```ocaml
module type Comparable = sig
  type t                    (* Abstract type *)
  val compare : t -> t -> int
  val eq : t -> t -> bool
end
```

### Signature Items

| Item | Description |
|------|-------------|
| `type t` | Abstract type (hidden) |
| `type t = ...` | Concrete type (revealed) |
| `val x : ty` | Value with type |
| `module M : S` | Submodule with signature |
| `include S` | Include another signature |

## Module Structures

A **structure** implements a signature:

```ocaml
module IntComparable : Comparable = struct
  type t = int
  let compare x y = if x < y then -1 else if x > y then 1 else 0
  let eq x y = x = y
end

(* Or without annotation (compiler infers) *)
module IntComparable = struct
  type t = int
  let compare = Stdlib.compare
  let eq x y = x = y
end
```

## Type Abstraction

Abstract types hide implementation:

```ocaml
(* Signature with abstract type *)
module type Stack = sig
  type t              (* Abstract - implementation hidden! *)
  val empty : t
  val push : int -> t -> t
  val pop : t -> (int * t) option
end

(* Implementation with list *)
module ListStack : Stack = struct
  type t = int list     (* Hidden from users! *)
  let empty = []
  let push x s = x :: s
  let pop = function
    | [] -> None
    | x :: xs -> Some (x, xs)
end

(* Users cannot rely on t = int list *)
(* They can only use the provided operations *)
```

## Functors

A **functor** is a function from modules to modules:

```ocaml
module MakeBST (E : Comparable) = struct
  type elt = E.t

  type t =
    | Empty
    | Node of elt * t * t

  let empty = Empty

  let rec add x = function
    | Empty -> Node (x, Empty, Empty)
    | Node (v, left, right) ->
        if E.compare x v < 0
        then Node (v, add x left, right)
        else if E.compare x v > 0
        then Node (v, left, add x right)
        else Node (v, left, right)

  let rec mem x = function
    | Empty -> false
    | Node (v, left, right) ->
        E.compare x v = 0
        || (E.compare x v < 0 && mem x left)
        || mem x right
end
```

### Functor Application

```ocaml
module IntComparable = struct
  type t = int
  let compare = Stdlib.compare
  let eq x y = x = y
end

module StringComparable = struct
  type t = string
  let compare = Stdlib.compare
  let eq x y = x = y
end

(* Instantiate functor for int *)
module IntSet = MakeBST (IntComparable)

(* Instantiate functor for string *)
module StringSet = MakeBST (StringComparable)
```

### Usage

```ocaml
(* Int operations *)
let ints = IntSet.add 3 (IntSet.add 1 (IntSet.add 5 IntSet.empty))
IntSet.mem 3 ints  (* true *)
IntSet.mem 7 ints  (* false *)

(* String operations *)
let strs = StringSet.add "banana" (StringSet.add "apple" StringSet.empty)
StringSet.mem "apple" strs  (* true *)
```

## Higher-Order Functors

Functors can take functors as arguments:

```ocaml
module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(* Monad transformer functor *)
module OptionT (M : Monad) = struct
  type 'a t = 'a option M.t

  let return x = M.return (Some x)

  let bind m f =
    M.bind m (function
      | None -> M.return None
      | Some x -> f x)

  let lift mx = M.map (fun x -> Some x) mx
end
```

## Standard Library Functors

OCaml's standard library provides many functors:

| Functor | Purpose | Argument |
|---------|---------|----------|
| `Make` | Hash table | `Hashable` |
| `Map.Make` | Balanced tree | `OrderedType` |
| `Set.Make` | Sets | `OrderedType` |
| `Hashtbl.Make` | Hash tables | `HashedType` |

### Example: Map.Make

```ocaml
module StringMap = Map.Make (String)

let m = StringMap.empty
let m = StringMap.add "key1" 42 m
let m = StringMap.add "key2" 100 m
StringMap.find "key1" m  (* 42 *)
```

### Example: Set.Make

```ocaml
module IntSet = Set.Make (struct
  type t = int
  let compare = Stdlib.compare
end)

let s = IntSet.add 3 (IntSet.add 1 IntSet.empty)
IntSet.elements s  (* [1; 3] *)
```

## Sharing Constraints

Sometimes you need to relate types across modules:

```ocaml
module type S = sig
  type t
end

module A : S = struct type t = int end
module B : S = struct type t = int end

(* Error: A.t and B.t are different! *)

(* Solution: sharing constraint *)
module C = sig
  type t
  type u = A.t  (* u shares with A.t *)
end
```

## Module System in Other Languages

| Language | Feature | Similar To |
|----------|---------|------------|
| OCaml | Modules/Functors | ML modules |
| Haskell | Type classes | Ad-hoc polymorphism |
| Rust | Traits | Interface + impl |
| Java | Interfaces + Generics | Limited modules |
| Scala | Traits + Implicits | Closest to ML |

## Modules vs Objects

| Aspect | Modules | Objects |
|--------|---------|---------|
| Binary operation | Compile-time | Run-time |
| Type parameters | Functors | Generics |
| Dispatch | Static | Dynamic |
| Nesting | Native | Inner classes |

## Try It

```ocaml
(* Define a comparable type *)
module type Comparable = sig
  type t
  val compare : t -> t -> int
end

(* BST functor *)
module MakeBST (E : Comparable) = struct
  type elt = E.t
  type t = Empty | Node of elt * t * t
  let empty = Empty
  let rec add x = function
    | Empty -> Node (x, Empty, Empty)
    | Node (v, l, r) ->
        let c = E.compare x v in
        if c < 0 then Node (v, add x l, r)
        else if c > 0 then Node (v, l, add x r)
        else Node (v, l, r)
end

(* Use it *)
module Int = struct
  type t = int
  let compare = Stdlib.compare
end

module IntSet = MakeBST (Int)
```

## Exercises

1. Implement a `MakeQueue` functor with `enqueue` and `dequeue`
2. Implement `MakeParser` functor that takes a token type
3. Create a functor that generates monadic transformers
4. Implement a `MakeRingBuffer` functor
5. Build a dependency graph using modules

## Further Reading

- *Real World OCaml*, Chapter 9 - Functors
- [OCaml Modules](https://ocaml.org/manual/modules.html) - OCaml Manual
- *Introduction to Standard ML*, Modules chapter
- [Modular Programming](https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/modules/modules.html) - Cornell CS3110

## Real-World Connection

- **Jane Street Core**: Extensive use of functors for infrastructure
- **Menhir/OCamlLex**: Compiler tools built with functors
- **Containers**: Alternative stdlib with better functor usage
- **MirageOS**: Unikernel built with modular functors

## Next Lesson

[Lesson 13: Effects and Monads](lesson13.md)
