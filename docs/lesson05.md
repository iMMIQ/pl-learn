# Lesson 05: Continuations and CPS

**Learning Objectives:**
- Understand continuation-passing style (CPS)
- Implement CPS transformation
- Learn about control operators (call/cc)
- See how CPS enables advanced control flow

## What is a Continuation?

A **continuation** represents "the rest of the computation":

```ocaml
(* In evaluating (3 + 4) * 5 *)
(* After computing 3 + 4 = 7, the continuation is: *)
(* fun v -> v * 5 *)
```

## Direct Style vs CPS

**Direct style:**
```ocaml
let add x y = x + y
let result = add 3 4  (* returns 7 *)
```

**CPS:**
```ocaml
let add_cps x y k = k (x + y)
let result = add_cps 3 4 (fun v -> v)  (* passes 7 to continuation *)
```

## CPS Transformation Rules

```
[[x]]         = x
[[n]]         = n
[[e1 + e2]]   = let x1 = [[e1]] in
                let x2 = [[e2]] in
                +(x1, x2)
[[let x = e1 in e2]]
              = let x1 = [[e1]] in [[e2]]
```

## Example Transformation

**Original:**
```
(3 + 4) * 5
```

**CPS:**
```
let x_1 = 3 in
let x_2 = 4 in
let x_3 = +(x_1, x_2) in
let x_4 = 5 in
+(x_3, x_4)
```

## Control Operators

### call/cc (call-with-current-continuation)

Captures the current continuation as a first-class value:

```scheme
(call/cc (λk. (+ 1 (k 2))))  ; Returns 2, not 3
```

### Exceptions with Continuations

```ocaml
(* Raising an exception means invoking a captured continuation *)
(* catch e (handler ... where k captures the escape point) *)
```

## Applications of CPS

1. **Compilers**: Eliminates call stack, enables tail call optimization
2. **Exceptions**: Jump to handler without stack unwinding
3. **Coroutines**: Swap continuations to switch contexts
4. **Backtracking**: Save/restore computation state
5. **Async/await**: Continuations as callbacks

## Try It

```ocaml
open Lesson05_cps;;

(* Transform to CPS *)
let e = Add (Const 3, Const 4);;
let cps_e = cps_transform e;;

(* Print CPS form *)
string_of_cps_expr cps_e;;
(* "let x_1 = 3 in let x_2 = 4 in +(x_1, x_2)" *)

(* Evaluate *)
eval_cps (fun _ -> raise Not_found) cps_e;;
(* 7 *)
```

## Exercises

1. Implement `catch` and `throw` using continuations
2. Add subtraction to the CPS language
3. Implement coroutines (swap continuation)
4. Prove that CPS transformation preserves semantics

## Further Reading

- *Continuations and Control Flow*, Friedman & Wand
- [Call/cc](https://en.wikipedia.org/wiki/Call-with-current-continuation)
- [CPS in Compilers](https://www.cs.princeton.edu/courses/archive/fall03/cs527/handouts/cps.pdf)

## Course Complete!

Congratulations! You've completed the PL Learn course.

## What's Next?

- Study type systems more deeply (subtyping, polymorphism)
- Explore implementation techniques (garbage collection, compilation)
- Read *Types and Programming Languages* cover to cover
- Implement a real programming language!

## Resources

- [TaPL](https://www.cis.upenn.edu/~bcpierce/tapl/) - Types and Programming Languages
- [EOPL](https://www.eopl3.com/) - Essentials of Programming Languages
- [PLAI](https://plt.eecs.northwestern.edu/books/) - Programming Languages: Application and Interpretation
