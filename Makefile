.PHONY: all clean test bench doc examples

all:
	dune build

test:
	dune runtest -j1 --no-buffer

bench:
	dune build @bench

examples:
	dune build @examples

clean:
	dune clean

doc:
	dune build @doc
