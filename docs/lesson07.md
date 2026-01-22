# Lesson 07: Abstract Machines

**Learning Objectives:**
- Understand abstract machine design
- Learn stack-based evaluation
- Implement closures and environments
- Bridge theory and implementation

## What is an Abstract Machine?

An **abstract machine** is a computational model that sits between high-level language semantics and low-level hardware implementation. It makes explicit:

- **Control flow** (how execution proceeds)
- **Memory management** (environments, closures)
- **Operation sequencing** (step-by-step execution)

## Why Abstract Machines?

```
High-level Language
        ↓
    [Abstract Machine]  ← Structural Operational Semantics
        ↓
    Concrete Machine
```

**Benefits:**
1. **Precise execution model** - No ambiguity
2. **Compilation target** - Real compilers use this
3. **Optimization opportunities** - Transform the machine code
4. **Correctness proofs** - Easier than direct compilation

## The SECD Machine

The SECD machine was designed by Peter Landin (1964) for lambda calculus.

### Machine State

```
State = (S, E, C, D)

S - Stack:   [v1, v2, v3, ...]
E - Env:     [frame1, frame2, ...]
C - Control: [instr1, instr2, ...]
D - Dump:    [(S,E,C), (S,E,C), ...]  (saved states)
```

### Instructions

| Instruction | Meaning |
|-------------|---------|
| `Const n` | Push integer n onto stack |
| `Access i` | Push variable from environment |
| `Close code` | Create closure from code |
| `App` | Apply function |
| `Return` | Return from function |
| `Add` | Add top two stack values |

### Execution Example

```
(λx. x + 1) 5

Initial:
  S = []
  E = []
  C = [Close, Const 1, Close, Const 5, App]
  D = []

After first Close (create λx.x+1):
  S = [<closure>]
  E = []
  C = [Const 1, Close, Const 5, App]

... (omitted steps)

Final:
  S = [6]
  E = []
  C = []
```

### Key Insight: The Dump

The **dump** saves the current state before entering a function. This is how we implement **function return** without a hardware call stack!

## Environment Machine (CES)

A simpler machine focusing on **closures** and **environments**.

### Machine State

```
State = (C, E, S)

C - Code:     [instr1, instr2, ...]
E - Env:      [(x1,v1), (x2,v2), ...]
S - Stack:    [v1, v2, v3, ...]
```

### Closures

A **closure** packages function code with its environment:

```ocaml
type closure = {
  param : string;           (* Function parameter *)
  body : instr list;        (* Function body *)
  env : (string * value) list;  (* Captured environment *)
}
```

### Execution Model

```ocaml
(* Function application *)
match stack with
| VClos {param; body; env} :: arg :: rest ->
    (* Extend environment with parameter binding *)
    new_env = (param, arg) :: env
    (* Continue with function body *)
    new_state = {
      code = body;
      env = new_env;
      stack = rest;
    }
```

## Comparison

| Aspect | SECD | Environment Machine |
|--------|------|-------------------|
| Complexity | Higher (4 registers) | Lower (3 registers) |
| Explicit returns | Yes (via dump) | Implicit |
| Closure handling | Special instruction | Direct |
| Memory usage | More (dump) | Less |
| Real-world use | Academic | Common (Lua, etc.) |

## Implementation: From Lambda to Machine

### Step 1: Compile

```ocaml
let rec compile = function
  | Const n -> [IConst n]
  | Var x -> [IVar x]
  | Abs (x, e) -> [IAbs (x, compile e)]
  | App (e1, e2) -> compile e1 @ compile e2 @ [IApp]
```

### Step 2: Execute

```ocaml
let rec eval state =
  match state.code with
  | [] -> top_of_stack state
  | instr :: rest ->
      state |> execute instr |> eval
```

## Try It

```ocaml
open Lesson07_env_machine;;

(* Identity function *)
let e = App (Abs ("x", Var "x"), Const 42);;
eval e;;
(* VInt 42 *)

(* Higher-order *)
let e = App (
  Abs ("f", App (Var "f", Const 3)),
  Abs ("x", Add (Var "x", Const 2))
);;
eval e;;
(* VInt 5 *)

(* Trace execution *)
trace e;;
(* Shows each step of machine execution *)
```

## Exercises

1. Add subtraction and multiplication to the machine
2. Implement proper variable access with depth/index
3. Add a `Fix` instruction for recursion
4. Extend to support pairs (tuples)
5. Compare performance: SECD vs Environment

## Further Reading

- *The Mechanical Evaluation of Expressions*, Landin (1964)
- *Abstract Machines for Programming Languages*, Leroy (1990)
- [SECD Machine Wikipedia](https://en.wikipedia.org/wiki/SECD_machine)

## Real-World Connection

- **BEAM (Erlang VM)**: Register-based abstract machine
- **Lua VM**: Register-based, similar concepts
- **OCaml bytecode**: ZAM (Zinc Abstract Machine)
- **JVM**: Stack-based, inspired by these ideas

## Next Lesson

[Lesson 08: Subtyping](lesson08.md) - Subtype relations and variance
