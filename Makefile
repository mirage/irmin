.PHONY: all clean test fuzz bench-type bench-mem bench-pack bench doc examples

all:
	dune build

test:
	dune runtest

bench-type:
	dune exec ./bench/irmin/main.exe -- --output-dir _build/default/data

bench-mem:
	dune exec ./test/irmin-mem/bench.exe

bench-pack:
	dune exec ./test/irmin-pack/bench.exe

bench: bench-type bench-mem bench-pack

fuzz:
	dune build @fuzz --no-buffer

examples:
	dune build @examples

clean:
	dune clean

doc:
	dune build @doc
