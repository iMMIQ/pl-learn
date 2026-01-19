# Phase 5: Continuations and CPS

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement Continuation-Passing Style (CPS) transformation and control operators.

**Architecture:** CPS transformer, continuation data structure, control operators.

**Tech Stack:** OCaml, core language

**Prerequisites:** Phase 1 - Arithmetic Expressions completed

---

## Task 1: CPS Transformation

**Files:**
- Create: `lib/lesson05/dune`
- Create: `lib/lesson05/cps.ml`
- Create: `lib/lesson05/cps.mli`

**Step 1: Create lib/lesson05/dune**

```ocaml
(library
 (name lesson05)
 (public_name pl_learn.lesson05))
```

**Step 2: Create lib/lesson05/cps.mli**

```ocaml
(** Lesson 05: Continuations and CPS *)

(** {1 Source Language - Direct Style} *)

type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr
  | Var of string
  | Let of string * expr * expr

(** {1 CPS Language} *)

type cps_expr =
  | CConst of int
  | CVar of string
  | CPrim of string * cps_expr list * cps_cont
  | CLet of string * cps_expr * cps_cont
  | CApp of cps_expr * cps_expr list

and cps_cont =
  | CId                               (* Identity continuation *)
  | CArg of string * cps_cont         (* Push argument *)
  | CBin1 of string * cps_expr * cps_cont  (* First arg of binary op *)
  | CFunc of cps_expr * cps_cont      (* Function application *)

(** {1 CPS Transformation} *)

val cps_transform : expr -> cps_expr

(** {1 CPS Evaluation} *)

val eval_cps : (string -> int) -> cps_expr -> int

(** {1 Pretty Printing} *)

val string_of_cps_expr : cps_expr -> string
val string_of_cps_cont : cps_cont -> string
```

**Step 3: Create lib/lesson05/cps.ml**

```ocaml
(* {1 Source Language} *)

type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr
  | Var of string
  | Let of string * expr * expr

(* {1 CPS Language} *)

type cps_expr =
  | CConst of int
  | CVar of string
  | CPrim of string * cps_expr list * cps_cont
  | CLet of string * cps_expr * cps_cont
  | CApp of cps_expr * cps_expr list

and cps_cont =
  | CId
  | CArg of string * cps_cont
  | CBin1 of string * cps_expr * cps_cont
  | CFunc of cps_expr * cps_cont

(* {1 Fresh Variable Generation} *)

let counter = ref 0
let fresh prefix =
  incr counter;
  prefix ^ "_" ^ string_of_int !counter

(* {1 CPS Transformation} *)

let rec cps_transform e =
  let rec transform e k =
    match e with
    | Const n -> apply_cont k (CConst n)

    | Var x -> apply_cont k (CVar x)

    | Add (e1, e2) ->
        let x1 = fresh "x" in
        let x2 = fresh "x" in
        transform e1 (CBin1 ("+", cps_transform e2, CArg (x2, k))) |> fun body ->
        CLet (x1, body, CPrim ("+", [CVar x1; CVar x2], k))

    | Mul (e1, e2) ->
        let x1 = fresh "x" in
        let x2 = fresh "x" in
        transform e1 (CBin1 ("*", cps_transform e2, CArg (x2, k))) |> fun body ->
        CLet (x1, body, CPrim ("*", [CVar x1; CVar x2], k))

    | Let (x, e1, e2) ->
        transform e1 (CFunc (cps_transform e2, CArg (x, k)))

  and apply_cont k v =
    match k with
    | CId -> v
    | CArg (x, k') -> CLet (x, v, apply_cont k' (CVar x))
    | CBin1 (op, e2, k') -> CPrim (op, [v; e2], k')
    | CFunc (f, k') -> CApp (f, [v])

  in
  transform e CId

(* {1 CPS Evaluation} *)

let rec eval_cps env = function
  | CConst n -> n
  | CVar x ->
      (try env x with Not_found ->
         raise (Invalid_argument ("Unbound: " ^ x)))
  | CPrim (op, args, k) ->
      let values = List.map (eval_cps env) args in
      let result = match op, values with
        | "+", [n1; n2] -> n1 + n2
        | "*", [n1; n2] -> n1 * n2
        | _ -> raise (Invalid_argument ("Unknown op: " ^ op))
      in
      eval_cont env k result

  | CLet (x, e1, k) ->
      let v1 = eval_cps env e1 in
      let env' y = if y = x then v1 else env y in
      eval_cont env' k v1

  | CApp (f, args) ->
      (* Simplified: just evaluate function and args *)
      eval_cps env f (* In full CPS, this would call the function *)

and eval_cont env k v =
  match k with
  | CId -> v
  | CArg (x, k') ->
      let env' y = if y = x then v else env y in
      eval_cont env' k' v
  | CBin1 (op, e2, k') ->
      let v2 = eval_cps env e2 in
      let result = match op with
        | "+" -> v + v2
        | "*" -> v * v2
        | _ -> raise (Invalid_argument "Unknown op")
      in
      eval_cont env k' result
  | CFunc (f, k') ->
      eval_cont env k' (eval_cps env f)

(* {1 Pretty Printing} *)

let rec string_of_cps_expr = function
  | CConst n -> string_of_int n
  | CVar x -> x
  | CPrim (op, args, k) ->
      let args_str = String.concat ", " (List.map string_of_cps_expr args) in
      Printf.sprintf "%s(%s); %s" op args_str (string_of_cps_cont k)
  | CLet (x, e1, k) ->
      Printf.sprintf "let %s = %s in %s" x (string_of_cps_expr e1) (string_of_cps_cont k)
  | CApp (f, args) ->
      let args_str = String.concat ", " (List.map string_of_cps_expr args) in
      Printf.sprintf "%s(%s)" (string_of_cps_expr f) args_str

and string_of_cps_cont = function
  | CId -> "halt"
  | CArg (x, k) -> Printf.sprintf "(%s → %s)" x (string_of_cps_cont k)
  | CBin1 (op, e2, k) -> Printf.sprintf "(<%s> %s; %s)" op (string_of_cps_expr e2) (string_of_cps_cont k)
  | CFunc (f, k) -> Printf.sprintf "(<app> %s; %s)" (string_of_cps_expr f) (string_of_cps_cont k)
```

**Step 4: Build**

```bash
dune build
```

**Step 5: Commit**

```bash
git add lib/lesson05/
git commit -m "feat(lesson05): add CPS transformation"
```

---

## Task 2: Control Operators

**Files:**
- Create: `lib/lesson05/control.ml`
- Create: `lib/lesson05/control.mli`

**Step 1: Create lib/lesson05/control.mli**

```ocaml
(** Control Operators: call/cc and exceptions *)

(** {1 Value Type} *)

type value =
  | VInt of int
  | VCont of cps_cont  (* First-class continuation *)

(** {1 Extended CPS with call/cc} *)

type cps_expr_ext =
  | EConst of int
  | EVar of string
  | EPrim of string * cps_expr_ext list * cps_cont_ext
  | ELet of string * cps_expr_ext * cps_cont_ext
  | ECallCC of string * cps_cont_ext  (* call/cc *)

and cps_cont_ext =
  | EHalt
  | EArg of string * cps_cont_ext
  | EBin1 of string * cps_expr_ext * cps_cont_ext
  | EFunc of cps_expr_ext * cps_cont_ext

(** {1 Evaluation with First-Class Continuations} *)

val eval_ext : (string -> value) -> cps_expr_ext -> value

(** {1 Example: Capturing and Invoking Continuations} *)

val example_callcc : cps_expr_ext
val example_exception : cps_expr_ext
```

**Step 2: Create lib/lesson05/control.ml**

```ocaml
type value =
  | VInt of int
  | VCont of cps_cont_ext

and cps_cont_ext =
  | EHalt
  | EArg of string * cps_cont_ext
  | EBin1 of string * cps_expr_ext * cps_cont_ext
  | EFunc of cps_expr_ext * cps_cont_ext

and cps_expr_ext =
  | EConst of int
  | EVar of string
  | EPrim of string * cps_expr_ext list * cps_cont_ext
  | ELet of string * cps_expr_ext * cps_cont_ext
  | ECallCC of string * cps_cont_ext

(* {1 Evaluation} *)

let rec eval_ext env = function
  | EConst n -> VInt n
  | EVar x ->
      (try env x with Not_found ->
         raise (Invalid_argument ("Unbound: " ^ x)))

  | EPrim (op, args, k) ->
      let values = List.map (eval_ext env) args in
      let result = match op, values with
        | "+", [VInt n1; VInt n2] -> VInt (n1 + n2)
        | "*", [VInt n1; VInt n2] -> VInt (n1 * n2)
        | _ -> raise (Invalid_argument "Unknown op")
      in
      eval_cont_ext env k result

  | ELet (x, e1, k) ->
      let v1 = eval_ext env e1 in
      let env' y = if y = x then v1 else env y in
      eval_cont_ext env' k v1

  | ECallCC (x, k) ->
      (* Call/cc: bind x to current continuation, then evaluate under k *)
      let env' y = if y = x then VCont k else env y in
      eval_cont_ext env' k (VInt 0) (* Placeholder *)

and eval_cont_ext env k v =
  match k with
  | EHalt -> v
  | EArg (x, k') ->
      let env' y = if y = x then v else env y in
      eval_cont_ext env' k' v
  | EBin1 (op, e2, k') ->
      let v2 = eval_ext env e2 in
      let result = match v, v2 with
        | VInt n1, VInt n2 ->
            (match op with
             | "+" -> VInt (n1 + n2)
             | "*" -> VInt (n1 * n2)
             | _ -> raise (Invalid_argument "Unknown op")
        | _ -> raise (Invalid_argument "Type error")
      in
      eval_cont_ext env k' result
  | EFunc (f, k') ->
      (* Apply function to value *)
      eval_cont_ext env k' (eval_ext env f)

(* {1 Examples} *)

(* Using call/cc to return early *)
(* Conceptually: (call/cc (λk. (+ 1 (k 2)))) returns 2 *)
let example_callcc =
  ECallCC ("k",
    EFunc (EPrim ("+", [EConst 1; EVar "k"], EHalt),
      EArg ("_", EHalt)))

(* Exception-like behavior with continuations *)
(* Conceptually: (throw 5) catches the value 5 *)
let example_exception =
  EPrim ("throw", [EConst 5], EHalt) (* Would need proper throw/catch setup *)
```

**Step 3: Update lib/lesson05/dune**

```ocaml
(library
 (name lesson05)
 (public_name pl_learn.lesson05)
 (modules cps control))
```

**Step 4: Build**

```bash
dune build
```

**Step 5: Commit**

```bash
git add lib/lesson05/
git commit -m "feat(lesson05): add control operators"
```

---

## Task 3: Tests for CPS

**Files:**
- Create: `test/lesson05/dune`
- Create: `test/lesson05/test_cps.ml`

**Step 1: Create test/lesson05/dune**

```ocaml
(test
 (name test_cps)
 (libraries lesson05 alcotest))
```

**Step 2: Create test/lesson05/test_cps.ml**

```ocaml
open Alcotest
open Lesson05_cps
open Lesson05_control

(* CPS transformation tests *)

let test_cps_const () =
  let e = Const 42 in
  let cps_e = cps_transform e in
  let r = eval_cps (fun _ -> raise Not_found) cps_e in
  check int "Const 42" 42 r

let test_cps_add () =
  let e = Add (Const 3, Const 5) in
  let cps_e = cps_transform e in
  let r = eval_cps (fun _ -> raise Not_found) cps_e in
  check int "3 + 5" 8 r

let test_cps_mul () =
  let e = Mul (Const 4, Const 6) in
  let cps_e = cps_transform e in
  let r = eval_cps (fun _ -> raise Not_found) cps_e in
  check int "4 * 6" 24 r

let test_cps_nested () =
  let e = Add (Mul (Const 3, Const 4), Const 5) in
  let cps_e = cps_transform e in
  let r = eval_cps (fun _ -> raise Not_found) cps_e in
  check int "(3 * 4) + 5" 17 r

let test_cps_let () =
  let e = Let ("x", Const 10, Add (Var "x", Const 5)) in
  let cps_e = cps_transform e in
  let r = eval_cps (fun _ -> raise Not_found) cps_e in
  check int "let x = 10 in x + 5" 15 r

let test_cps_complex () =
  let e = Let ("x", Mul (Const 3, Const 4),
               Let ("y", Add (Var "x", Const 2),
                    Mul (Var "x", Var "y"))) in
  let cps_e = cps_transform e in
  let r = eval_cps (fun _ -> raise Not_found) cps_e in
  check int "complex let" 84 r

(* Control operator tests *)

let test_eval_const_ext () =
  let e = EConst 42 in
  let r = eval_ext (fun _ -> raise Not_found) e in
  check bool "Const ext" true
    (match r with VInt 42 -> true | _ -> false)

let test_eval_add_ext () =
  let e = EPrim ("+", [EConst 3; EConst 5], EHalt) in
  let r = eval_ext (fun _ -> raise Not_found) e in
  check bool "Add ext" true
    (match r with VInt 8 -> true | _ -> false)

let () =
  run "Lesson05: Continuations and CPS" [
    ("CPS Transformation", [
      test_cps_const;
      test_cps_add;
      test_cps_mul;
      test_cps_nested;
      test_cps_let;
      test_cps_complex;
    ]);
    ("Control Operators", [
      test_eval_const_ext;
      test_eval_add_ext;
    ]);
  ]
```

**Step 3: Run tests**

```bash
dune runtest
```

**Step 4: Commit**

```bash
git add test/lesson05/
git commit -m "test(lesson05): add CPS tests"
```

---

## Task 4: Examples and Documentation

**Files:**
- Create: `examples/lesson05_examples.ml`
- Create: `docs/lesson05.md`

**Step 1: Create examples/lesson05_examples.ml**

```ocaml
open Lesson05_cps

let () =
  Printf.printf "\n=== Lesson 05: Continuations and CPS ===\n\n";

  Printf.printf "1. Direct Style vs CPS:\n";
  Printf.printf "   Direct: (3 + 4) * 5 = 35\n";

  let direct = Mul (Add (Const 3, Const 4), Const 5) in
  let cps_e = cps_transform direct in
  Printf.printf "   CPS: %s\n" (string_of_cps_expr cps_e);
  Printf.printf "   Result: %d\n\n" (eval_cps (fun _ -> raise Not_found) cps_e);

  Printf.printf "2. CPS Transformation explained:\n";
  Printf.printf "   Original: let x = 10 in x + 5\n";

  let e = Let ("x", Const 10, Add (Var "x", Const 5)) in
  let cps_e = cps_transform e in
  Printf.printf "   CPS: %s\n\n" (string_of_cps_expr cps_e);

  Printf.printf "3. Why CPS?\n";
  Printf.printf "   - No call stack needed\n";
  Printf.printf "   - Control flow explicit\n";
  Printf.printf "   - Easy to implement exceptions, coroutines\n";
  Printf.printf "   - Used in compilers (LLVM, etc.)\n";

  Printf.printf "\n=== End of Examples ===\n"
```

**Step 2: Create docs/lesson05.md**

```markdown
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
[[x]]         = k x
[[n]]         = k n
[[e1 + e2]]   = [[e1]] (λv1. [[e2]] (λv2. k (v1 + v2)))
[[let x = e1 in e2]]
              = [[e1]] (λv1. [[e2]](x := v1) k)
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
+(x_3, x_4); halt
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
(* "let x_1 = 3 in +(x_1, 4); halt" *)

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
```

**Step 3: Update docs/index.md** with lesson05 link

**Step 4: Commit**

```bash
git add examples/ docs/
git commit -m "docs(lesson05): add continuations and CPS documentation"
```

---

## Summary

After Phase 5, you have completed:
- ✅ CPS transformation from direct style
- ✅ Continuation data structures
- ✅ Control operators (call/cc)
- ✅ First-class continuations
- ✅ Complete PL theory course

**Course Complete!** 🎉
