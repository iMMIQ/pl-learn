# Phase 0: Project Setup

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Initialize the PL Learn project with build system, directory structure, and basic configuration.

**Architecture:** Standard OCaml project layout using dune build system.

**Tech Stack:** OCaml 5.x, dune 3.16+, menhir, alcotest

---

## Task 1: Create Basic Project Files

**Files:**
- Create: `dune-project`
- Create: `Makefile`
- Create: `.gitignore`
- Create: `README.md`
- Create: `.ocamlformat`

**Step 1: Create dune-project**

```ocaml
(lang dune 3.16)
(name pl_learn)
(package
 (name pl_learn)
 (synopsis "Programming Language Theory - Interactive Learning"))

(dependencies
  (ocaml (>= 5.0))
  (menhir (>= 20231231))
  (alcotest (>= 1.8.0)))
```

**Step 2: Create Makefile**

```makefile
.PHONY: all test clean docs fmt

all:
	dune build

test:
	dune runtest

clean:
	dune clean

docs:
	dune build @doc

fmt:
	dune fmt

repl:
	dune utop lib
```

**Step 3: Create .gitignore**

```
_build/
*.exe
*.native
*.byte
*.cmi
*.cmo
*.cmx
*.o
*.a
*.so
*.dune-deps
*.install
*.merlin
_dune/
```

**Step 4: Create README.md**

```markdown
# PL Learn - Programming Language Theory Interactive Course

Learn Programming Language theory through hands-on implementation in OCaml.

## Getting Started

```bash
# Install dependencies
opam install dune menhir alcotest ocaml-lalr

# Build
make

# Run tests
make test

# Start REPL
make repl
```

## Course Structure

| Phase | Topic |
|-------|-------|
| 0 | Project Setup |
| 1 | Arithmetic Expressions |
| 2 | Lambda Calculus |
| 3 | Simply Typed λ-Calculus |
| 4 | Operational Semantics |
| 5 | Continuations |

## License

MIT
```

**Step 5: Create .ocamlformat**

```ocaml
version=0.26.2
profile=conventional
```

**Step 6: Verify structure**

```bash
ls -la
```

**Step 7: Initialize git and commit**

```bash
git init
git add dune-project Makefile .gitignore README.md .ocamlformat
git commit -m "chore: initialize project structure"
```

---

## Task 2: Create Directory Structure

**Files:**
- Create: `lib/dune`
- Create: `test/dune`
- Create: `examples/dune`
- Create: `bin/dune`
- Create: `docs/plans/.gitkeep`

**Step 1: Create lib/dune**

```ocaml
(library
 (name pl_learn)
 (public_name pl_learn.lib))
```

**Step 2: Create test/dune**

```ocaml
(alias
 (name runtest)
 (action
  (run ${<})))
```

**Step 3: Create examples/dune**

```ocaml
(executable
 (name main)
 (public_name pl_learn_examples)
 (libraries pl_learn))
```

**Step 4: Create bin/dune**

```ocaml
(executable
 (name main)
 (public_name pl_learn)
 (libraries pl_learn))
```

**Step 5: Create placeholder bin/main.ml**

```ocaml
let () =
  Printf.printf "PL Learn - Programming Language Theory\n";
  Printf.printf "Coming soon...\n"
```

**Step 6: Create examples/main.ml**

```ocaml
let () =
  Printf.printf "Run examples here\n"
```

**Step 7: Build and verify**

```bash
dune build
```

Expected: Successful compilation

**Step 8: Commit**

```bash
git add -A
git commit -m "chore: create directory structure"
```

---

## Summary

After this phase, you have:
- ✅ OCaml project with dune build system
- ✅ Standard directory structure (lib/, test/, examples/, bin/, docs/)
- ✅ Makefile with common commands
- ✅ Git repository initialized

**Next:** Proceed to Phase 1 - Arithmetic Expressions
