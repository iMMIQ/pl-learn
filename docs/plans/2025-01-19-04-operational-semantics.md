# Phase 4: Operational Semantics

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement small-step and big-step operational semantics with tracing.

**Architecture:** Separate semantics modules, transition traces, visualization.

**Tech Stack:** OCaml, core language

**Prerequisites:** Phase 1 - Arithmetic Expressions completed

---

## Task 1: Small-Step Semantics

**Files:**
- Create: `lib/lesson04/dune`
- Create: `lib/lesson04/smallstep.ml`
- Create: `lib/lesson04/smallstep.mli`

**Step 1: Create lib/lesson04/dune**

```ocaml
(library
 (name lesson04)
 (public_name pl_learn.lesson04))
```

**Step 2: Create lib/lesson04/smallstep.mli**

```ocaml
(** Lesson 04: Small-Step Operational Semantics *)

(** {1 Abstract Syntax} *)

type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr
  | Var of string
  | Let of string * expr * expr

(** {1 Values} *)

type value =
  | VInt of int

(** {1 Single Step Reduction} *)

(** [step e] computes one small-step reduction.
    Returns [Some e'] if e → e', [None] if e is a value.
*)
val step : expr -> expr option

(** {1 Multi-Step Reduction} *)

(** [steps e] returns the complete reduction sequence to normal form. *)
val steps : expr -> expr list

(** [reduce e] computes the normal form using small-step semantics. *)
val reduce : ?max_steps:int -> expr -> expr option

(** {1 Trace Visualization} *)

val trace : expr -> string
```

**Step 3: Create lib/lesson04/smallstep.ml**

```ocaml
type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr
  | Var of string
  | Let of string * expr * expr

type value =
  | VInt of int

let is_value = function
  | Const _ -> true
  | _ -> false

(* {1 Single Step Reduction} *)

let rec step = function
  (* Add *)
  | Add (Const n1, Const n2) ->
      Some (Const (n1 + n2))
  | Add (Const n1, e2) when is_value e2 ->
      None (* stuck - shouldn't happen with well-formed terms *)
  | Add (e1, e2) ->
      (match step e1 with
       | Some e1' -> Some (Add (e1', e2))
       | None when is_value e1 ->
           (match step e2 with
            | Some e2' -> Some (Add (e1, e2'))
            | None -> None)
       | None -> Some (Add (e1, e2)))

  (* Mul *)
  | Mul (Const n1, Const n2) ->
      Some (Const (n1 * n2))
  | Mul (e1, e2) ->
      (match step e1 with
       | Some e1' -> Some (Mul (e1', e2))
       | None when is_value e1 ->
           (match step e2 with
            | Some e2' -> Some (Mul (e1, e2'))
            | None -> None)
       | None -> Some (Mul (e1, e2)))

  (* Let *)
  | Let (x, Const v, e2) ->
      Some (subst x v e2)
  | Let (x, e1, e2) ->
      (match step e1 with
       | Some e1' -> Some (Let (x, e1', e2))
       | None -> None)

  (* Values - no reduction *)
  | Const _ -> None

  (* Variables - stuck unless substituted *)
  | Var _ -> None

and subst x v = function
  | Const n -> Const n
  | Add (e1, e2) -> Add (subst x v e1, subst x v e2)
  | Mul (e1, e2) -> Mul (subst x v e1, subst x v e2)
  | Var y -> if y = x then Const v else Var y
  | Let (y, e1, e2) ->
      Let (y, subst x v e1, if y = x then e2 else subst x v e2)

(* {1 Multi-Step Reduction} *)

let rec string_of_expr = function
  | Const n -> string_of_int n
  | Add (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
  | Mul (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " * " ^ string_of_expr e2 ^ ")"
  | Var x -> x
  | Let (x, e1, e2) ->
      "let " ^ x ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2

let steps e =
  let rec loop e acc =
    match step e with
    | None -> List.rev (e :: acc)
    | Some e' -> loop e' (e :: acc)
  in
  loop e []

let reduce ?(max_steps=1000) e =
  let rec loop e count =
    if count >= max_steps then None
    else
      match step e with
      | None -> Some e
      | Some e' -> loop e' (count + 1)
  in
  loop e 0

let trace e =
  let seq = steps e in
  String.concat "\n  → " (List.map string_of_expr seq)
```

**Step 4: Build**

```bash
dune build
```

**Step 5: Commit**

```bash
git add lib/lesson04/
git commit -m "feat(lesson04): add small-step semantics"
```

---

## Task 2: Big-Step Semantics

**Files:**
- Create: `lib/lesson04/bigstep.ml`
- Create: `lib/lesson04/bigstep.mli`

**Step 1: Create lib/lesson04/bigstep.mli**

```ocaml
(** Lesson 04: Big-Step Operational Semantics *)

(** {1 Big-Step Evaluation} *)

(** [eval env e] computes the value of expression [e] using big-step semantics.
    The relation is written: env ⊢ e ⇓ v
*)
val eval : (string -> int) -> expr -> int

(** {1 Evaluation derivation tree} *)

type derivation =
  | DConst of int
  | DAdd of derivation * derivation * int
  | DMul of derivation * derivation * int
  | DVar of string * int
  | DLet of string * derivation * derivation * int

(** [eval_derivation env e] computes both value and derivation tree. *)
val eval_derivation : (string -> int) -> expr -> int * derivation

(** {1 Pretty Print Derivation} *)

val pp_derivation : Format.formatter -> derivation -> unit
val string_of_derivation : derivation -> string
```

**Step 2: Create lib/lesson04/bigstep.ml**

```ocaml
(* Reuse expression type from smallstep - in practice, share via common module *)
type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr
  | Var of string
  | Let of string * expr * expr

type derivation =
  | DConst of int
  | DAdd of derivation * derivation * int
  | DMul of derivation * derivation * int
  | DVar of string * int
  | DLet of string * derivation * derivation * int

let rec eval env = function
  | Const n -> n
  | Add (e1, e2) -> eval env e1 + eval env e2
  | Mul (e1, e2) -> eval env e1 * eval env e2
  | Var x ->
      (try env x with Not_found ->
         raise (Invalid_argument ("Unbound: " ^ x)))
  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      let env' y = if y = x then v1 else env y in
      eval env' e2

let rec eval_derivation env = function
  | Const n -> (n, DConst n)
  | Add (e1, e2) ->
      let (v1, d1) = eval_derivation env e1 in
      let (v2, d2) = eval_derivation env e2 in
      (v1 + v2, DAdd (d1, d2, v1 + v2))
  | Mul (e1, e2) ->
      let (v1, d1) = eval_derivation env e1 in
      let (v2, d2) = eval_derivation env e2 in
      (v1 * v2, DMul (d1, d2, v1 * v2))
  | Var x ->
      let v = env x in
      (v, DVar (x, v))
  | Let (x, e1, e2) ->
      let (v1, d1) = eval_derivation env e1 in
      let env' y = if y = x then v1 else env y in
      let (v2, d2) = eval_derivation env' e2 in
      (v2, DLet (x, d1, d2, v2))

let rec string_of_derivation indent = function
  | DConst n ->
      indent ^ "── Const(" ^ string_of_int n ^ ") ⇓ " ^ string_of_int n
  | DVar (x, v) ->
      indent ^ "── Var(" ^ x ^ ") ⇓ " ^ string_of_int v
  | DAdd (d1, d2, v) ->
      indent ^ "── Add(" ^ string_of_int v ^ ")\n" ^
      string_of_derivation (indent ^ "  ") d1 ^ "\n" ^
      string_of_derivation (indent ^ "  ") d2 ^ "\n" ^
      indent ^ "── ⇓ " ^ string_of_int v
  | DMul (d1, d2, v) ->
      indent ^ "── Mul(" ^ string_of_int v ^ ")\n" ^
      string_of_derivation (indent ^ "  ") d1 ^ "\n" ^
      string_of_derivation (indent ^ "  ") d2 ^ "\n" ^
      indent ^ "── ⇓ " ^ string_of_int v
  | DLet (x, d1, d2, v) ->
      indent ^ "── Let(" ^ x ^ ", " ^ string_of_int v ^ ")\n" ^
      string_of_derivation (indent ^ "  ") d1 ^ "\n" ^
      string_of_derivation (indent ^ "  ") d2 ^ "\n" ^
      indent ^ "── ⇓ " ^ string_of_int v

let pp_derivation fmt d =
  Format.pp_print_string fmt (string_of_derivation "" d)

let string_of_derivation = string_of_derivation ""
```

**Step 3: Update lib/lesson04/dune**

Add bigstep to modules:
```ocaml
(library
 (name lesson04)
 (public_name pl_learn.lesson04)
 (modules smallstep bigstep))
```

**Step 4: Build**

```bash
dune build
```

**Step 5: Commit**

```bash
git add lib/lesson04/
git commit -m "feat(lesson04): add big-step semantics with derivation trees"
```

---

## Task 3: Tests for Semantics

**Files:**
- Create: `test/lesson04/dune`
- Create: `test/lesson04/test_semantics.ml`

**Step 1: Create test/lesson04/dune**

```ocaml
(test
 (name test_semantics)
 (libraries lesson04 alcotest))
```

**Step 2: Create test/lesson04/test_semantics.ml**

```ocaml
open Alcotest
open Lesson04_smallstep
open Lesson04_bigstep

(* Small-step tests *)

let test_step_add_consts () =
  let e = Add (Const 3, Const 5) in
  let r = step e in
  check bool "3+5 → 8"
    (match r with Some (Const 8) -> true | _ -> false)

let test_step_nested () =
  let e = Add (Add (Const 1, Const 2), Const 3) in
  let r = step e in
  check bool "(1+2)+3 → 3+3"
    (match r with Some (Add (Const 3, Const 3)) -> true | _ -> false)

let test_steps_trace () =
  let e = Add (Add (Const 1, Const 2), Const 3) in
  let seq = steps e in
  check int "trace length" 3 (List.length seq)

let test_reduce_normal_form () =
  let e = Add (Mul (Const 2, Const 3), Const 4) in
  let r = reduce e in
  check bool "2*3+4 = 10"
    (match r with Some (Const 10) -> true | _ -> false)

let test_step_let () =
  let e = Let ("x", Const 5, Add (Var "x", Const 3)) in
  let r = step e in
  check bool "let x=5 in x+3 → 5+3"
    (match r with Some (Add (Const 5, Const 3)) -> true | _ -> false)

(* Big-step tests *)

let test_bigstep_const () =
  let r = eval (fun _ -> raise Not_found) (Const 42) in
  check int "Const 42 ⇓ 42" 42 r

let test_bigstep_add () =
  let e = Add (Const 3, Const 5) in
  let r = eval (fun _ -> raise Not_found) e in
  check int "3+5 ⇓ 8" 8 r

let test_bigstep_let () =
  let e = Let ("x", Const 10, Add (Var "x", Const 5)) in
  let r = eval (fun _ -> raise Not_found) e in
  check int "let x=10 in x+5 ⇓ 15" 15 r

let test_bigstep_nested () =
  let e = Let ("x", Const 5,
               Let ("y", Const 3,
                    Mul (Var "x", Var "y"))) in
  let r = eval (fun _ -> raise Not_found) e in
  check int "nested let" 15 r

let () =
  run "Lesson04: Operational Semantics" [
    ("Small-Step", [
      test_step_add_consts;
      test_step_nested;
      test_steps_trace;
      test_reduce_normal_form;
      test_step_let;
    ]);
    ("Big-Step", [
      test_bigstep_const;
      test_bigstep_add;
      test_bigstep_let;
      test_bigstep_nested;
    ]);
  ]
```

**Step 3: Run tests**

```bash
dune runtest
```

**Step 4: Commit**

```bash
git add test/lesson04/
git commit -m "test(lesson04): add operational semantics tests"
```

---

## Task 4: Examples and Documentation

**Files:**
- Create: `examples/lesson04_examples.ml`
- Create: `docs/lesson04.md`

**Step 1: Create examples/lesson04_examples.ml**

```ocaml
open Lesson04_smallstep
open Lesson04_bigstep

let () =
  Printf.printf "\n=== Lesson 04: Operational Semantics ===\n\n";

  Printf.printf "1. Small-Step Reduction:\n";
  Printf.printf "   (1 + 2) + 3:\n";
  let e = Add (Add (Const 1, Const 2), Const 3) in
  Printf.printf "   %s\n\n" (trace e);

  Printf.printf "2. Let Binding:\n";
  let e = Let ("x", Add (Const 3, Const 4), Mul (Var "x", Const 2)) in
  Printf.printf "   %s\n\n" (trace e);

  Printf.printf "3. Big-Step Derivation:\n";
  let e = Add (Mul (Const 2, Const 3), Const 4) in
  let (_, deriv) = eval_derivation (fun _ -> raise Not_found) e in
  Printf.printf "   %s\n\n" (string_of_derivation deriv);

  Printf.printf "4. Comparison:\n";
  Printf.printf "   Small-step: step-by-step computation\n";
  Printf.printf "   Big-step: direct value with derivation tree\n";

  Printf.printf "\n=== End of Examples ===\n"
```

**Step 2: Create docs/lesson04.md**

```markdown
# Lesson 04: Operational Semantics

**Learning Objectives:**
- Understand small-step vs big-step semantics
- Write inference rules for evaluation
- Trace program execution
- Prove correctness properties

## Small-Step Semantics

Small-step semantics defines computation as a sequence of single reductions:

```
e → e' → e'' → ... → v
```

### Inference Rules

```
  n1 + n2 = n
  ───────────────────  (S-AddConst)
  Const n1 + Const n2 → Const n

     e1 → e1'
  ────────────────────  (S-Add1)
  e1 + e2 → e1' + e2

  e1 is a value    e2 → e2'
  ─────────────────────────  (S-Add2)
       e1 + e2 → e1 + e2'
```

## Big-Step Semantics

Big-step semantics defines direct evaluation to a value:

```
env ⊢ e ⇓ v
```

### Inference Rules

```
  ─────────────────  (B-Const)
  env ⊢ Const n ⇓ n

  env ⊢ e1 ⇓ n1    env ⊢ e2 ⇓ n2    n = n1 + n2
  ───────────────────────────────────────────  (B-Add)
          env ⊢ e1 + e2 ⇓ n

  env ⊢ e1 ⇓ v1    env, x:=v1 ⊢ e2 ⇓ v2
  ───────────────────────────────────  (B-Let)
      env ⊢ let x = e1 in e2 ⇓ v2
```

## Comparison

| Aspect | Small-Step | Big-Step |
|--------|-----------|----------|
| Focus | How computation proceeds | What the result is |
| Intermediate states | Explicit | Hidden in derivation |
| Non-terminating | Divergence visible | No derivation |
| Use cases | Concurrency, progress | Proofs, compilation |

## Examples

Small-step trace for `(1 + 2) + 3`:
```
(1 + 2) + 3
→ 3 + 3
→ 6
```

Big-step derivation:
```
  ── Const(1) ⇓ 1     ── Const(2) ⇓ 2
  ────────────────────────────────── Add(3)
  ── Const(3) ⇓ 3
  ────────────────────────────────── Add(6)
  ── ⇓ 6
```

## Try It

```ocaml
open Lesson04_smallstep;;

let e = Add (Add (Const 1, Const 2), Const 3);;
trace e;;
(* "(1 + 2) + 3
    → 3 + 3
    → 6" *)

open Lesson04_bigstep;;

let (_, d) = eval_derivation (fun _ -> raise Not_found) e;;
string_of_derivation d;;
```

## Exercises

1. Add subtraction and division to both semantics
2. Prove that small-step and big-step are equivalent
3. Implement a "debug" mode that shows each reduction step
4. Add exceptions and error handling

## Further Reading

- *Operational Semantics*, Lecture Notes - Harper
- [Essentials of Programming Languages](https://www.eopl3.com/)

## Next Lesson

[Lesson 05: Continuations](lesson05.md)
```

**Step 3: Update docs/index.md** and examples/dune

**Step 4: Commit**

```bash
git add examples/ docs/
git commit -m "docs(lesson04): add operational semantics documentation"
```

---

## Summary

After Phase 4, you have:
- ✅ Small-step semantics with single-step reduction
- ✅ Big-step semantics with derivation trees
- ✅ Tracing and visualization
- ✅ Comparison between approaches
- ✅ Tests and examples

**Next:** Phase 5 - Continuations
