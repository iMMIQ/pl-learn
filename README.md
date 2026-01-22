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

# Run examples (by lesson)
dune exec lesson01_examples
dune exec lesson02_examples
# ... etc
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
| 6 | Type Inference |
| 7 | Abstract Machines |
| 8 | Subtyping |
| 9 | Garbage Collection |

## Project Layout

```
pl-learn/
├── bin/           # Main executable
├── lib/           # Library code (by lesson)
│   ├── lesson01/  # Arithmetic expressions
│   ├── lesson02/  # Lambda calculus
│   └── ...
├── test/          # Test suites (by lesson)
│   ├── lesson01/
│   └── ...
├── examples/      # Example programs (by lesson)
│   ├── lesson01/
│   └── ...
└── docs/          # Documentation and plans
```

## License

MIT
