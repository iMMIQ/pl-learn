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
open Lesson04.Smallstep;;

let e = Add (Add (Const 1, Const 2), Const 3);;
trace e;;
(* "(1 + 2) + 3
    → 3 + 3
    → 6" *)

open Lesson04.Bigstep;;

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
