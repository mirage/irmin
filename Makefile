.PHONY: all clean test fuzz bench-pack bench-layers bench doc examples

all:
	dune build

test:
	dune runtest

bench-layers:
	@dune exec -- ./bench/irmin-pack/layers.exe -n 2005 -b 2 -j

bench: bench-layers

fuzz:
	dune build @fuzz --no-buffer

examples:
	dune build @examples

clean:
	dune clean

doc:
	dune build @doc
