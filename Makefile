.PHONY: all clean test

all:
	dune build

test:
	dune runtest -j1 --no-buffer

examples:
	dune build @examples

clean:
	dune clean
