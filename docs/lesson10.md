# Lesson 10: Lazy Evaluation and Call-by-Need

**Learning Objectives:**
- Understand strict vs lazy evaluation
- Implement thunks and delayed computation
- Learn call-by-name vs call-by-need
- Work with infinite data structures
- Understand space leaks

## What is Lazy Evaluation?

**Lazy evaluation** delays computation until a value is actually needed.

### Comparison

| Strategy | When to Evaluate? | Multiple Evaluations? |
|----------|-------------------|----------------------|
| Call-by-value (strict) | Before calling function | No |
| Call-by-name | When value is needed | Yes (recomputes) |
| Call-by-need | When value is needed | No (memoizes) |

## Thunks

A **thunk** is a delayed computation:

```ocaml
(* Create a thunk *)
let t = make_lazy (fun () -> expensive_computation ())

(* Force evaluation *)
let v = get t

(* Already memoized - no recomputation *)
let v2 = get t
```

### Thunk States

```
┌─────────────────────────────────────┐
│  Unevaluated  ──force──►  Evaluated │
│  (computation)                      │
└─────────────────────────────────────┘
        ↓                         ↓
   first call             cached value
```

## Call-by-Name vs Call-by-Need

### Call-by-Name

```ocaml
(* Arguments are not evaluated *)
(* (λx. 1) (divergent)  → 1 *)
(* x is never used, so (divergent) is never evaluated *)
```

### Call-by-Need (Memoized)

```ocaml
(* Arguments are evaluated at most once *)
(* (λx. x + x) (expensive) → expensive + expensive *)
(* But (expensive) is only computed once, result reused *)
```

## Infinite Data Structures

Lazy evaluation enables infinite data structures:

```ocaml
(* Infinite stream of natural numbers *)
let rec nats n = Cons (n, lazy (nats (n + 1)))

(* Take first 10 *)
take 10 (nats 0)  (* [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] *)
```

### Fibonacci Sequence

```ocaml
(* Infinite fibs using lazy streams *)
let rec fibs_aux a b =
  Cons (a, lazy (fibs_aux b (a + b)))

let fibs = fibs_aux 0 1

(* First 10: [0; 1; 1; 2; 3; 5; 8; 13; 21; 34] *)
```

### Sieve of Eratosthenes

```ocaml
let rec sieve = function
  | Cons (p, rest) ->
      let no_multiples = filter ((<>) 0 mod p) (force rest) in
      Cons (p, lazy (sieve no_multiples))

(* Infinite primes! *)
let primes = sieve (nats_from 2)
```

## Advantages of Lazy Evaluation

1. **Modularity**: Separate control from data
   ```ocaml
   (* Define infinite data *)
   let all_numbers = nats 0

   (* Consumer decides how many to take *)
   take 10 all_numbers
   ```

2. **Efficiency**: Avoid unnecessary computation
   ```ocaml
   (* if condition then expensive_value else cheap_value *)
   (* Only one branch is evaluated *)
   ```

3. **Infinite structures**: Work with conceptually infinite data
   ```ocaml
   let primes = sieve (nats_from 2)
   take 100 primes  (* First 100 primes *)
   ```

4. **Data-driven control flow**
   ```ocaml
   (* Short-circuit operators work naturally *)
   (* true || divergent → true (no error) *)
   ```

## Disadvantages

1. **Space leaks**: Retained thunks consume memory
   ```ocaml
   (* Accumulates thunks instead of values *)
   let sum_bad n = fold_left (+) 0 (take n (nats 0))
   ```

2. **Performance unpredictability**: Hard to reason about when things run

3. **Debugging difficulty**: Stack traces don't match source code

4. **Interaction with side effects**: Order of execution unclear

## Space Leaks

A common pitfall in lazy evaluation:

```ocaml
(* BAD: Accumulates thunks *)
let bad_sum =
  let rec aux n acc =
    if n <= 0 then acc
    else aux (n - 1) (Cons (n, lazy acc))  (* Thunk builds up! *)
  in
  aux 1000000 Nil

(* GOOD: Force evaluation in loop *)
let good_sum n =
  let rec aux i acc =
    if i <= 0 then acc
    else aux (i - 1) (acc + i)
  in
  aux n 0
```

## Lazy Evaluation in Real Languages

| Language | Default | Optional | Notes |
|----------|---------|----------|-------|
| Haskell | Lazy | - | Pure lazy language |
| OCaml | Strict | `lazy` keyword | Opt-in lazy |
| Scala | Strict | `=>` (by-name), `LazyVal` | Mixed |
| Racket | Strict | `lazy`/`force` | Opt-in |
| F# | Strict | `Lazy` module | Opt-in |
| Swift | Strict | `lazy` keyword | Opt-in |

## Strictness Analysis

Compilers for lazy languages use **strictness analysis** to optimize:

```haskell
-- Compiler sees that f always uses its argument
f :: Int -> Int
f x = x + 1

-- Optimizes to: evaluate x before calling f
```

## Try It

```ocaml
module S = Lesson10.Stream

(* Create infinite stream *)
let naturals = S.nats 0

(* Take first 10 *)
S.take 10 naturals
(* [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] *)

(* Get primes *)
S.take 10 S.primes
(* [2; 3; 5; 7; 11; 13; 17; 19; 23; 29] *)

(* Fibonacci *)
S.take 15 S.fibs
(* [0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233; 377] *)
```

## Exercises

1. Implement a lazy `repeat` function that repeats a value forever
2. Implement `iterate f x` that generates `[x; f x; f (f x); ...]`
3. Create a lazy quicksort that works on infinite lists
4. Implement `unfold` as dual of `fold`
5. Detect and fix space leaks in lazy code

## Further Reading

- *Introduction to Functional Programming*, Bird & Wadler - Chapter on lazy evaluation
- *The Implementation of Functional Programming Languages*, Peyton Jones
- *Why Functional Programming Matters*, Hughes (shows power of lazy evaluation)

## Real-World Connection

- **Haskell**: Pure lazy language - everything is lazy by default
- **OCaml**: `Lazy` module for opt-in laziness
- **Streaming libraries**: Lazy evaluation for processing large data
- **Reactive programming**: Lazy propagation of changes

## Next Lesson

[Lesson 11: Pattern Matching and Algebraic Data Types](lesson11.md)
