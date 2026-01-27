.PHONY: all clean test fuzz bench bench-fast bench-full doc examples

all:
	dune build

test:
	dune runtest

bench-fast:
	dune build @bench-fast

bench-full:
	dune build @bench-full

bench: bench-fast

fuzz:
	dune build @fuzz --no-buffer

examples:
	dune build @examples

clean:
	dune clean

doc:
	dune build @doc
