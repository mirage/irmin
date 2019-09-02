.PHONY: all clean test doc examples

all:
	dune build

test:
	dune runtest

examples:
	dune build @examples

clean:
	dune clean

doc:
	dune build @doc
