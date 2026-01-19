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
