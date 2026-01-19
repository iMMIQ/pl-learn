# Phase 3: Simply Typed λ-Calculus

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement simply typed lambda calculus with type checking and base types.

**Architecture:** Type inference with explicit type annotations, bidirectional checking.

**Tech Stack:** OCaml, core language

**Prerequisites:** Phase 2 - Lambda Calculus completed

---

## Task 1: Define Types and Typed Terms

**Files:**
- Create: `lib/lesson03/dune`
- Create: `lib/lesson03/typed.ml`
- Create: `lib/lesson03/typed.mli`

**Step 1: Create lib/lesson03/dune**

```ocaml
(library
 (name lesson03)
 (public_name pl_learn.lesson03))
```

**Step 2: Create lib/lesson03/typed.mli**

```ocaml
(** Lesson 03: Simply Typed Lambda Calculus *)

(** {1 Types} *)

type typ =
  | TyBool                            (* Boolean type *)
  | TyNat                             (* Natural number type *)
  | TyArrow of typ * typ              (* Function type: τ1 → τ2 *)

(** {1 Typed Expressions} *)

type expr =
  | TmVar of string * typ             (* Variable with type *)
  | TmAbs of string * typ * expr      (* Abstraction: λx:τ. e *)
  | TmApp of expr * expr              (* Application: e1 e2 *)
  | TmTrue                            (* true *)
  | TmFalse                           (* false *)
  | TmIf of expr * expr * expr        (* if e1 then e2 else e3 *)
  | TmZero                            (* 0 *)
  | TSucc of expr                     (* succ e *)
  | TPred of expr                     (* pred e *)
  | TmIsZero of expr                  (* iszero e *)

(** {1 Type Checking} *)

exception TypeError of string

val typeof : (string -> typ option) -> expr -> typ
val typecheck : expr -> typ

(** {1 Evaluation} *)

val eval : expr -> expr

(** {1 Pretty Printing} *)

val string_of_typ : typ -> string
val string_of_expr : expr -> string
```

**Step 3: Create lib/lesson03/typed.ml**

```ocaml
type typ =
  | TyBool
  | TyNat
  | TyArrow of typ * typ

type expr =
  | TmVar of string * typ
  | TmAbs of string * typ * expr
  | TmApp of expr * expr
  | TmTrue
  | TmFalse
  | TmIf of expr * expr * expr
  | TmZero
  | TSucc of expr
  | TPred of expr
  | TmIsZero of expr

exception TypeError of string

(* {1 Type Checking} *)

let rec typeof env = function
  | TmVar (x, _) ->
      (try env x |> Option.get
       with Not_found -> raise (TypeError ("Unbound: " ^ x)))

  | TmAbs (x, ty1, e2) ->
      let env' y = if y = x then Some ty1 else env y in
      let ty2 = typeof env' e2 in
      TyArrow (ty1, ty2)

  | TmApp (e1, e2) ->
      let ty1 = typeof env e1 in
      let ty2 = typeof env e2 in
      (match ty1 with
       | TyArrow (ty11, ty12) ->
           if ty2 = ty11 then ty12
           else raise (TypeError "Parameter type mismatch")
       | _ -> raise (TypeError "Expected arrow type"))

  | TmTrue -> TyBool
  | TmFalse -> TyBool

  | TmIf (e1, e2, e3) ->
      if typeof env e1 <> TyBool then
        raise (TypeError "if condition must be bool");
      let ty2 = typeof env e2 in
      let ty3 = typeof env e3 in
      if ty2 = ty3 then ty2
      else raise (TypeError "Branches must have same type")

  | TmZero -> TyNat

  | TSucc e ->
      if typeof env e = TyNat then TyNat
      else raise (TypeError "succ expects nat")

  | TPred e ->
      if typeof env e = TyNat then TyNat
      else raise (TypeError "pred expects nat")

  | TmIsZero e ->
      if typeof env e = TyNat then TyBool
      else raise (TypeError "iszero expects nat")

let typecheck e = typeof (fun _ -> None) e

(* {1 Evaluation} *)

let rec eval = function
  | TmApp (TmAbs (_, _, body), arg) ->
      eval (subst 0 arg body)
  | TmApp (e1, e2) ->
      TmApp (eval e1, eval e2)
  | TmIf (TmTrue, e2, _) -> eval e2
  | TmIf (TmFalse, _, e3) -> eval e3
  | TmIf (e1, e2, e3) -> TmIf (eval e1, e2, e3)
  | TSucc e -> TSucc (eval e)
  | TPred e -> TPred (eval e)
  | TmIsZero e -> TmIsZero (eval e)
  | e -> e

and subst i value = function
  | TmVar (x, _) when x = "_" && i = 0 -> value
  | TmVar (x, ty) -> TmVar (x, ty)
  | TmAbs (x, ty, body) ->
      TmAbs (x, ty, if x = "_" then subst (i + 1) value body else body)
  | TmApp (e1, e2) -> TmApp (subst i value e1, subst i value e2)
  | TmTrue -> TmTrue
  | TmFalse -> TmFalse
  | TmIf (e1, e2, e3) -> TmIf (subst i value e1, subst i value e2, subst i value e3)
  | TmZero -> TmZero
  | TSucc e -> TSucc (subst i value e)
  | TPred e -> TPred (subst i value e)
  | TmIsZero e -> TmIsZero (subst i value e)

(* {1 Pretty Printing} *)

let rec string_of_typ = function
  | TyBool -> "Bool"
  | TyNat -> "Nat"
  | TyArrow (t1, t2) ->
      let left = match t1 with
        | TyArrow _ -> "(" ^ string_of_typ t1 ^ ")"
        | _ -> string_of_typ t1
      in
      left ^ " → " ^ string_of_typ t2

let rec string_of_expr = function
  | TmVar (x, _) -> x
  | TmAbs (x, ty, e) ->
      "λ" ^ x ^ ":" ^ string_of_typ ty ^ ". " ^ string_of_expr e
  | TmApp (e1, e2) ->
      let left = match e1 with
        | TmAbs _ -> "(" ^ string_of_expr e1 ^ ")"
        | _ -> string_of_expr e1
      in
      left ^ " " ^ string_of_expr e2
  | TmTrue -> "true"
  | TmFalse -> "false"
  | TmIf (e1, e2, e3) ->
      "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^
      " else " ^ string_of_expr e3
  | TmZero -> "0"
  | TSucc e -> "succ " ^ string_of_expr e
  | TPred e -> "pred " ^ string_of_expr e
  | TmIsZero e -> "iszero " ^ string_of_expr e
```

**Step 4: Build**

```bash
dune build
```

**Step 5: Commit**

```bash
git add lib/lesson03/
git commit -m "feat(lesson03): add simply typed lambda calculus"
```

---

## Task 2: Write Type Checking Tests

**Files:**
- Create: `test/lesson03/dune`
- Create: `test/lesson03/test_typed.ml`

**Step 1: Create test/lesson03/dune**

```ocaml
(test
 (name test_typed)
 (libraries lesson03 alcotest))
```

**Step 2: Create test/lesson03/test_typed.ml**

```ocaml
open Alcotest
open Lesson03_typed

(* Basic types *)

let test_typeof_true () =
  check (option string) "true:Bool"
    (Some "Bool")
    (try Some (string_of_typ (typecheck TmTrue)) with _ -> None)

let test_typeof_zero () =
  check (option string) "0:Nat"
    (Some "Nat")
    (try Some (string_of_typ (typecheck TmZero)) with _ -> None)

let test_typeof_identity () =
  let e = TmAbs ("x", TyNat, TmVar ("x", TyNat)) in
  check (option string) "λx:Nat. x : Nat → Nat"
    (Some "Nat → Nat")
    (try Some (string_of_typ (typecheck e)) with _ -> None)

let test_typeof_arrow () =
  let e = TmAbs ("f", TyArrow (TyNat, TyNat),
                 TmVar ("f", TyArrow (TyNat, TyNat))) in
  check (option string) "λf:Nat→Nat. f"
    (Some "(Nat → Nat) → Nat → Nat")
    (try Some (string_of_typ (typecheck e)) with _ -> None)

let test_if_type () =
  let e = TmIf (TmTrue, TmFalse, TmTrue) in
  check (option string) "if true then false else true : Bool"
    (Some "Bool")
    (try Some (string_of_typ (typecheck e)) with _ -> None)

let test_succ_type () =
  let e = TSucc TmZero in
  check (option string) "succ 0 : Nat"
    (Some "Nat")
    (try Some (string_of_typ (typecheck e)) with _ -> None)

let test_iszero_type () =
  let e = TmIsZero TmZero in
  check (option string) "iszero 0 : Bool"
    (Some "Bool")
    (try Some (string_of_typ (typecheck e)) with _ -> None)

(* Type errors *)

let test_error_app_mismatch () =
  let e = TmApp (TmAbs ("x", TyNat, TmVar ("x", TyNat)), TmTrue) in
  raises "app mismatch" (TypeError "")
    (fun () -> typecheck e |> ignore)

let test_error_if_cond () =
  let e = TmIf (TmZero, TmTrue, TmFalse) in
  raises "if cond not bool" (TypeError "")
    (fun () -> typecheck e |> ignore)

let test_error_branches_mismatch () =
  let e = TmIf (TmTrue, TmTrue, TmZero) in
  raises "branches mismatch" (TypeError "")
    (fun () -> typecheck e |> ignore)

(* Evaluation *)

let test_eval_identity () =
  let e = TmApp (TmAbs ("x", TyNat, TmVar ("x", TyNat)), TmZero) in
  check bool "eval (λx:Nat. x) 0" true
    (match eval e with TmZero -> true | _ -> false)

let test_eval_if_true () =
  let e = TmIf (TmTrue, TmZero, TSucc TmZero) in
  check bool "eval if true then 0 else succ 0" true
    (match eval e with TmZero -> true | _ -> false)

let () =
  run "Lesson03: Simply Typed Lambda Calculus" [
    ("Type Checking", [
      test_typeof_true;
      test_typeof_zero;
      test_typeof_identity;
      test_typeof_arrow;
      test_if_type;
      test_succ_type;
      test_iszero_type;
    ]);
    ("Type Errors", [
      test_error_app_mismatch;
      test_error_if_cond;
      test_error_branches_mismatch;
    ]);
    ("Evaluation", [
      test_eval_identity;
      test_eval_if_true;
    ]);
  ]
```

**Step 3: Run tests**

```bash
dune runtest
```

**Step 4: Commit**

```bash
git add test/lesson03/
git commit -m "test(lesson03): add type checking tests"
```

---

## Task 3: Examples and Documentation

**Files:**
- Create: `examples/lesson03_examples.ml`
- Create: `docs/lesson03.md`

**Step 1: Create examples/lesson03_examples.ml**

```ocaml
open Lesson03_typed

let print_type e =
  try
    Printf.printf "  %s : %s\n"
      (string_of_expr e)
      (string_of_typ (typecheck e))
  with
  | TypeError msg -> Printf.printf "  Error: %s\n" msg

let () =
  Printf.printf "\n=== Lesson 03: Simply Typed λ-Calculus ===\n\n";

  Printf.printf "1. Base Types:\n";
  print_type TmTrue;
  print_type TmFalse;
  print_type TmZero;

  Printf.printf "\n2. Type Annotations:\n";
  print_type (TmAbs ("x", TyNat, TmVar ("x", TyNat)));
  print_type (TmAbs ("f", TyArrow (TyNat, TyBool),
                     TmApp (TmVar ("f", TyArrow (TyNat, TyBool)), TmZero)));

  Printf.printf "\n3. If Expression:\n";
  print_type (TmIf (TmTrue, TmFalse, TmTrue));

  Printf.printf "\n4. Arithmetic:\n";
  print_type (TSucc TmZero);
  print_type (TmIsZero TmZero);

  Printf.printf "\n5. Type Error - Mismatch:\n";
  print_type (TmApp (TmAbs ("x", TyNat, TmVar ("x", TyNat)), TmTrue));

  Printf.printf "\n=== End of Examples ===\n"
```

**Step 2: Update examples/dune**

Add library dependency:
```ocaml
(executable
 (name lesson03_examples)
 (public_name lesson03_examples)
 (libraries lesson03))
```

**Step 3: Create docs/lesson03.md**

```markdown
# Lesson 03: Simply Typed λ-Calculus

**Learning Objectives:**
- Understand simple types
- Implement type checking
- Learn type inference rules
- Understand type safety

## Types

```ocaml
type typ =
  | TyBool                    (* Boolean *)
  | TyNat                     (* Natural numbers *)
  | TyArrow of typ * typ      (* Functions: τ1 → τ2 *)
```

## Typed Expressions

Variables carry type annotations:
```ocaml
TmVar (x, τ)          (* x with type τ *)
TmAbs (x, τ, e)       (* λx:τ. e *)
TmApp (e1, e2)        (* e1 e2 *)
```

## Type Checking Rules

```
         x:τ ∈ Γ
  ──────────────────  (T-Var)
  Γ ⊢ x:τ

  Γ, x:τ1 ⊢ e:τ2
  ──────────────────────  (T-Abs)
  Γ ⊢ λx:τ1. e : τ1→τ2

  Γ ⊢ e1:τ11→τ12    Γ ⊢ e2:τ11
  ─────────────────────────────────  (T-App)
          Γ ⊢ e1 e2 : τ12

  ───────────  (T-True)   ───────────  (T-False)
  Γ ⊢ true:Bool          Γ ⊢ false:Bool

  Γ ⊢ e1:Bool    Γ ⊢ e2:τ    Γ ⊢ e3:τ
  ───────────────────────────────────────  (T-If)
        Γ ⊢ if e1 then e2 else e3 : τ
```

## Type Safety

**Progress**: Well-typed terms are either values or can take a step.

**Preservation**: If `Γ ⊢ e:τ` and `e → e'`, then `Γ ⊢ e':τ`.

## Examples

```ocaml
(* Identity function *)
λx:Nat. x  :  Nat → Nat

(* Function application *)
(λx:Nat. x) 0  :  Nat

(* Higher-order function *)
λf:Nat→Nat. f  :  (Nat → Nat) → Nat → Nat

(* Conditional *)
if true then false else true  :  Bool
```

## Try It

```ocaml
open Lesson03_typed;;

(* Type check an expression *)
let e = TmAbs ("x", TyNat, TmVar ("x", TyNat));;
typecheck e;;
(* TyArrow (TyNat, TyNat) *)

(* Type error *)
let bad = TmApp (TmAbs ("x", TyNat, TmVar ("x", TyNat)), TmTrue);;
typecheck bad;;
(* Exception: TypeError "Parameter type mismatch" *)
```

## Exercises

1. Add product types (pairs) `τ1 × τ2`
2. Add sum types (either) `τ1 + τ2`
3. Add list type `List τ`
4. Implement type reconstruction (inference)

## Further Reading

- *Types and Programming Languages*, Chapter 9 - Pierce
- [Type Systems](https://www.cis.upenn.edu/~bcpierce/tapl/) - Course notes

## Next Lesson

[Lesson 04: Operational Semantics](lesson04.md)
```

**Step 4: Update docs/index.md**

```markdown
3. [Simply Typed λ-Calculus](lesson03.md) - Type checking, type safety
```

**Step 5: Commit**

```bash
git add examples/ docs/
git commit -m "docs(lesson03): add typed lambda calculus documentation and examples"
```

---

## Summary

After Phase 3, you have:
- ✅ Simply typed lambda calculus
- ✅ Type checking with environments
- ✅ Base types (Bool, Nat) and functions
- ✅ Type safety (progress + preservation)
- ✅ Tests and examples

**Next:** Phase 4 - Operational Semantics
