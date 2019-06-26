.PHONY: all clean test bench doc examples

all:
	dune build

test:
	dune runtest

bench:
	dune build @bench

examples:
	dune build @examples

clean:
	dune clean

doc:
	dune build @doc
