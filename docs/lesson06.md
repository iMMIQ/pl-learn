# Lesson 06: Type Inference

**Learning Objectives:**
- Understand type variables and polymorphism
- Learn the unification algorithm
- Implement Hindley-Milner type inference
- Understand generalization and instantiation

## Motivation

In Lesson 3, we required explicit type annotations:

```ocaml
(* Simply Typed λ-Calculus - requires annotations *)
λx:Int. x
```

But ML and Haskell can infer types automatically:

```ocaml
(* ML - no annotations needed *)
let id = fun x -> x    (* id : 'a -> 'a *)
```

How does this work? **Type inference**!

## Type Variables

A type variable represents an unknown type:

```ocaml
type ty =
  | TyVar of string     (* 'a, 'b, 'c, ... *)
  | TyBool
  | TyInt
  | TyArrow of ty * ty  (* τ1 → τ2 *)
```

Examples:
- `'a` - unknown type
- `'a → 'a` - function from `'a` to `'a`
- `'a → 'b` - function from `'a` to `'b`

## Substitutions

A **substitution** maps type variables to types:

```
σ = ['a := Int, 'b := Bool]
```

Applying a substitution:

```
σ('a → 'b) = Int → Bool
σ('a → 'a) = Int → Int
```

Composition:

```
σ1 = ['a := Int]
σ2 = ['b := Bool]
σ1 ∘ σ2 = ['a := Int, 'b := Bool]
```

## Unification

**Unification** finds a substitution that makes two types equal:

```
unify('a, Int) = ['a := Int]
unify('a → 'b, Int → Bool) = ['a := Int, 'b := Bool]
unify(Int, Bool) = ERROR (cannot unify)
```

### Occurs Check

Prevents infinite types:

```
unify('a, 'a → 'b) = ERROR ('a occurs in 'a → 'b')
```

Without occurs check: `'a = 'a → 'b = ('a → 'b) → 'b = ...`

## Type Schemes

A **type scheme** represents polymorphic types:

```
∀'a. 'a → 'a    (* identity function *)
∀'a 'b. 'a → 'b → 'a    (* const function *)
```

In OCaml:

```ocaml
type scheme =
  | Mono of ty              (* Not polymorphic *)
  | Poly of string list * ty (* ∀vars. ty *)
```

## Generalization and Instantiation

**Generalization** (at let-bindings):

```ocaml
(* let id = λx. x in ... *)
(* Type of λx.x is 'a → 'a *)
(* Generalize over 'a (not in environment) *)
(* id : ∀'a. 'a → 'a *)
```

**Instantiation** (at variable use):

```ocaml
(* id 5 *)
(* Instantiate ∀'a. 'a → 'a with 'a := Int *)
(* Get Int → Int *)
```

## The Hindley-Milner Algorithm

```
Γ ⊢ e : τ

Γ ⊢ n : Int                        (T-Const)
Γ ⊢ true : Bool                    (T-Bool)
Γ(x) = σ    σ ≤ τ                   (T-Var)
Γ ⊢ x : instantiate(σ)

Γ, x:'a ⊢ e : τ                    (T-Abs)
Γ ⊢ λx. e : 'a → τ

Γ ⊢ e1 : τ1    Γ ⊢ e2 : τ2
unify(τ1, τ2 → 'a) = σ             (T-App)
Γ ⊢ e1 e2 : σ('a)

Γ ⊢ e1 : τ1    Γ, x:generalize(Γ,τ1) ⊢ e2 : τ2
Γ ⊢ let x = e1 in e2 : τ2          (T-Let)
```

## Examples

### Identity Function

```ocaml
λx. x
```

Inference steps:
1. Fresh type for `x`: `'a`
2. Body `x` has type `'a`
3. Result: `'a → 'a`
4. Generalize (at let): `∀'a. 'a → 'a`

### Function Application

```ocaml
(λx. x) 5
```

Inference steps:
1. `λx. x : 'a → 'a`
2. `5 : Int`
3. Unify: `'a → 'a` = `Int → 'b`
4. Get `'a := Int`
5. Result: `Int`

### Polymorphic let

```ocaml
let id = λx. x in (id 5, id true)
```

Inference steps:
1. `λx. x : 'a → 'a`
2. Generalize at let: `id : ∀'a. 'a → 'a`
3. For `id 5`: instantiate with `'a := Int`, get `Int → Int`
4. For `id true`: instantiate with `'a := Bool`, get `Bool → Bool`
5. Result types: `Int` and `Bool`

### Composition

```ocaml
λf. λg. λx. f (g x)
```

Type: `('b → 'c) → ('a → 'b) → 'a → 'c`

This shows how type inference captures higher-order structure!

## Try It

```ocaml
open Lesson06_inference;;

(* Identity *)
let id = EAbs ("x", EVar "x");;
typeof id;;
(* TyArrow (TyVar "'t1", TyVar "'t1") *)

(* Const function *)
let konst = EAbs ("x", EAbs ("y", EVar "x"));;
typeof konst;;
(* TyArrow (TyVar "'t2", TyArrow (TyVar "'t3", TyVar "'t2")) *)

(* Polymorphic let *)
let expr = ELet ("id", EAbs ("x", EVar "x"),
                EBinOp ("+", EApp (EVar "id", EConst 5),
                              EApp (EVar "id", EConst 3)));;
typeof expr;;
(* TyInt *)
```

## Exercises

1. Add product types (pairs) and their inference
2. Add sum types (Either) and their inference
3. Implement type class constraints
4. Add recursive types (μ)
5. Implement type annotations for disambiguation

## Further Reading

- *Principal Type-Schemes for Functional Programs*, Milner (1978)
- *Damas-Milner Type Inference*, original papers
- [Hindley-Milner Wikipedia](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)

## Next Lesson

[Lesson 07: Abstract Machines](lesson07.md) - SECD machine, environment-based evaluation
