# Lesson 14: Compiler Backend (Bytecode/VM)

**Learning Objectives:**
- Understand bytecode instruction sets
- Compile AST to bytecode
- Implement a stack-based VM
- Handle closures in bytecode
- Build a complete compiler

## What is a Compiler Backend?

A **compiler backend** transforms intermediate representations into executable code:

```
Source → Frontend (Parser) → IR → Backend (Codegen) → Machine Code
```

### Why Bytecode?

**Bytecode** is a low-level, compact representation:
- **Portable**: Runs on any platform with a VM
- **Compact**: Smaller than native code
- **Safe**: VM can enforce safety checks
- **Simple**: Easier to generate than native code

Examples: JVM (Java), BEAM (Erlang), CPython (Python), LuaVM

## Bytecode Instruction Set

Our stack-based VM uses these instructions:

| Instruction | Description | Stack Effect |
|-------------|-------------|--------------|
| `const n` | Push integer n | → n |
| `bool b` | Push boolean b | → b |
| `access d` | Access variable at depth d | → v |
| `closure addr n` | Create closure capturing n vars | → closure |
| `call n` | Call function with n args | args... → result |
| `return` | Return from function | result → |
| `pop` | Pop value | v → |
| `add/sub/mul/div` | Arithmetic ops | v2, v1 → result |
| `eq/lt/gt` | Comparisons | v2, v1 → bool |
| `jmp offset` | Unconditional jump | → |
| `jmpifz offset` | Jump if zero | v → |

## Stack-Based Execution

The VM uses a **stack** for intermediate values:

```
Expression: (3 + 4) * 5

Bytecode:
  const 3    -- Stack: [3]
  const 4    -- Stack: [3, 4]
  add        -- Stack: [7]
  const 5    -- Stack: [7, 5]
  mul        -- Stack: [35]
```

## Compilation Example

### Source Expression
```ocaml
let x = 10 in x + 5
```

### AST
```
ELet ("x", EInt 10, EPrim (Add, EVar "x", EInt 5))
```

### Bytecode
```
   0: const 10
   1: const 5
   2: access 0
   3: add
```

## Virtual Machine Architecture

```
┌─────────────────────────────────────────────┐
│  Virtual Machine                            │
│                                              │
│  ┌──────────┐  ┌────────────┐  ┌─────────┐  │
│  │ Code     │  │ Stack      │  │ Closures│  │
│  │ Memory   │  │ [bottom... │  │         │  │
│  │          │  │  ...top]   │  │         │  │
│  └──────────┘  └────────────┘  └─────────┘  │
│       ▲                                    │
│       │ PC                                 │
└─────────────────────────────────────────────┘
```

### VM State

```ocaml
type vm = {
  mutable code : instr array;  -- Code memory
  mutable pc : int;            -- Program counter
  mutable stack : value array; -- Value stack
  mutable sp : int;            -- Stack pointer
  mutable closures : ...;      -- Closure storage
}
```

## Real-World Bytecode VMs

| VM | Language | Architecture | Notes |
|----|----------|--------------|-------|
| JVM | Java | Stack-based | Type-annotated bytecode |
| BEAM | Erlang | Register-based | Concurrent per-process |
| CPython | Python | Stack-based | Rich opcode set |
| LuaVM | Lua | Register-based | Very compact |
| WASM | WebAssembly | Stack-based | Portable code format |

### Stack vs Register VMs

**Stack-based** (our VM, JVM):
- Simpler code generation
- Smaller bytecode
- More stack operations

**Register-based** (Lua, BEAM):
- Fewer instructions
- Better performance
- Larger bytecode

## Try It

```ocaml
open Lesson14__Ast

(* Compile and run *)
let prog = compile (
  ELet ("add", EAbs (["x"; "y"],
                EPrim (Add, EVar "x", EVar "y")),
        EApp (EVar "add", [EInt 10; EInt 20]))
)

let code = Lesson14__Bytecode.resolve_labels prog
let vm = create_vm code
let result = run_from_entry vm "main"
(* result = VInt 30 *)
```

## Exercises

1. Add subtraction instruction and test
2. Implement recursive factorial
3. Add string type and operations
4. Implement peephole optimizations
5. Add a "print" debugging instruction
6. Build a REPL that compiles and executes each line

## Further Reading

- *Design of VM*, Ernesto Aguierre
- [Lua 5.0 VM](https://lua.org/tsc.html) - Register-based design
- [JVM Spec](https://docs.oracle/javase/specs/jvms/se8/html/) - Stack-based VM
- *Crafting Interpreters*, Bob Nystrom - Chapter on code generation

## Real-World Connection

- **Java**: JVM bytecode + JIT compilation
- **Python**: CPython bytecode interpreter
- **JavaScript**: V8 (TurboFan), SpiderMonkey (IonMonkey)
- **Erlang**: BEAM VM for concurrent processes
- **WebAssembly**: Portable bytecode for web

## Course Complete!

You've now built:
- ✅ A functional language AST
- ✅ A bytecode compiler
- ✅ A stack-based VM
- ✅ Closure support
- ✅ A complete compilation pipeline

**Next Steps:**
- Study real compiler implementations (OCaml, LLVM)
- Add type checking to the compiler
- Implement optimizations
- Build a garbage collector for the VM
