.PHONY: all clean test fuzz bench-pack bench-layers bench doc examples

all:
	dune build

test:
	dune runtest

bench-pack:
	dune exec ./test/irmin-pack/bench.exe

bench-layers:
	dune exec -- ./bench/irmin-pack/layers.exe -n 2005 -b 2 -j

bench: bench-pack bench-layers

fuzz:
	dune build @fuzz --no-buffer

examples:
	dune build @examples

clean:
	dune clean

doc:
	dune build @doc
