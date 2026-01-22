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
open Lesson03.Typed;;

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
