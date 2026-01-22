# Lesson 08: Subtyping

**Learning Objectives:**
- Understand subtype relations
- Learn variance (covariant, contravariant)
- Implement record subtyping (width and depth)
- Understand the subsumption rule

## What is Subtyping?

A **subtype relation** `≤` (or `:<:`) between types allows one type to be used where another is expected.

```
τ1 ≤ τ2  means  "τ1 can be used where τ2 is expected"
```

### Intuition

```java
// In Java
Integer i = 5;
Number n = i;  // OK: Integer ≤ Number

List<Integer> ints = ...;
List<Number> nums = ints;  // ERROR! Not covariant
```

## Subtype Hierarchy

```
           Top (top of everything)
          /  |  \
        Bool Int String ...
          \  |  /
           Bot (bottom of everything)
```

- **Top**: Supertype of everything (often called `Any`, `Object`, `top`)
- **Bot**: Subtype of everything (often called `Nothing`, `bottom`, `⊥`)

## Subtype Rules

### Reflexivity

```
τ ≤ τ
```

Every type is a subtype of itself.

### Transitivity

```
τ1 ≤ τ2    τ2 ≤ τ3
────────────────────
      τ1 ≤ τ3
```

### Top and Bottom

```
Bot ≤ τ         for any τ
τ ≤ Top         for any τ
```

## Record Subtyping

Records have **two kinds** of subtyping:

### Width Subtyping

```
{l1:τ1, ..., ln:τn, l(n+1):τ(n+1)} ≤ {l1:τ1, ..., ln:τn}
```

A record with **more fields** is a subtype of a record with **fewer fields**.

**Why?** If you only use fields `x` and `y`, having extra fields doesn't matter.

```
{x:Int, y:Int, z:Bool} ≤ {x:Int, y:Int}
```

### Depth Subtyping

```
{l:τ1} ≤ {l:τ2}    if τ1 ≤ τ2
```

If the field types are subtypes, the records are subtypes.

```
{x:Int} ≤ {x:Top}    because Int ≤ Top
```

## Function Subtyping: Variance

Function types have **contravariant** parameters and **covariant** returns:

```
τ1 → τ2 ≤ σ1 → σ2    if σ1 ≤ τ1  and  τ2 ≤ σ2
                      ↑            ↑
                 contravariant  covariant
```

### Why Contravariant Parameters?

```
type Dog = Animal
type Cat = Animal

handler: Animal → Void
dog_handler: Dog → Void

Can we use dog_handler where handler is expected?
handler expects to handle ANY animal
dog_handler can only handle dogs
So: dog_handler is NOT a subtype of handler
But: handler IS a subtype of dog_handler!
```

In code:

```ocaml
(* Animal → String ≤ Dog → String *)
(* Because Animal is "larger" (supertype) than Dog *)
```

### Covariant Return

```ocaml
(* Dog → Dog ≤ Dog → Animal *)
(* Return type can be "larger" (supertype) *)
```

### Putting It Together

```ocaml
(* Top → Int ≤ Int → Bool *)
(* Top is "larger" than Int (contravariant domain) *)
(* Int ≤ Bool is false, so this doesn't work *)

(* Int → Top ≤ Top → Bot *)
(* This works! *)
```

## Join and Meet

For a **lattice** of types, we have:

### Join (∨) - Least Upper Bound

The smallest type that both types are subtypes of.

```
Int ∨ Bool = Top
Int ∨ Top = Top
{x:Int} ∨ {y:Bool} = {x:Int, y:Bool}
```

### Meet (∧) - Greatest Lower Bound

The largest type that is a subtype of both types.

```
Int ∧ Bool = Bot
Int ∧ Top = Int
{x:Int, y:Bool} ∧ {x:Int} = {x:Int}
```

## The Subsumption Rule

In type checking, subsumption allows "upcasting":

```
Γ ⊢ e : τ1    τ1 ≤ τ2
───────────────────── (T-Subsumption)
      Γ ⊢ e : τ2
```

This means if `e` has type `τ1` and `τ1 ≤ τ2`, then `e` also has type `τ2`.

## Examples

### Record Subtyping in Action

```ocaml
(* Function expecting {x: Int} *)
let f = fun (r: {x: int}) -> r.x

(* Pass {x: int, y: int} *)
let point = {x = 5; y = 3}

(* This works! {x:int, y:int} ≤ {x:int} *)
f point
```

### Function Variance

```ocaml
(* Animal → Dog ≤ Animal → Animal *)
(* Return type is covariant *)

(* Dog → Animal ≤ Animal → Animal *)
(* Parameter type is contravariant *)
```

## Variance Summary Table

| Type Constructor | Variance | Example |
|-----------------|----------|---------|
| Base types | - | Int not ≤ Bool |
| Record fields | Covariant | {x:Int} ≤ {x:Top} |
| Record width | Covariant (more ≤ fewer) | {x,y} ≤ {x} |
| Function domain | Contravariant | (Top→) ≤ (Int→) |
| Function codomain | Covariant | (→Int) ≤ (→Top) |
| Reference cells | Invariant | `ref Int` not ≤ `ref Top` |

## Try It

```ocaml
open Lesson08.Subtype;;

(* Basic checks *)
is_subtype TyInt TyTop;;
(* true *)

is_subtype (TyArrow (TyInt, TyInt)) (TyArrow (TyInt, TyTop));;
(* true - covariant return *)

is_subtype (TyArrow (TyTop, TyInt)) (TyArrow (TyInt, TyInt));;
(* true - contravariant parameter *)

(* Join and meet *)
join TyInt TyBool;;
(* TyTop *)

meet (TyRecord ["x", TyInt; "y", TyBool])
     (TyRecord ["x", TyInt]);;
(* TyRecord ["x", TyInt] *)
```

## Exercises

1. Add array types with covariance/contravariance
2. Implement union types with subtyping
3. Add bounded quantification (`<:T`)
4. Implement type case / downcasting
5. Explore recursive types with subtyping

## Further Reading

- *On Understanding Types, Data Abstraction, and Polymorphism*, Cardelli (1985)
- *A Theory of Object-Oriented Programming*, Abadi & Cardelli
- [Subtyping Wikipedia](https://en.wikipedia.org/wiki/Subtyping)

## Real-World Connection

- **Java**: Covariant arrays, invariant generics
- **C#**: Supports variance annotations (`in`/`out`)
- **OCaml**: No subtyping in core, but objects support it
- **TypeScript**: Structural subtyping (similar to our records)
