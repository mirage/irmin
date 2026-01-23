.PHONY: all clean test fuzz bench-pack bench bench-inline doc examples

all:
	dune build

test:
	dune runtest

bench-pack-with-trace-replay:
	@dune exec -- ./bench/irmin-pack/tree.exe --mode trace /home/opam/bench-dir/current-bench-data/mirage/irmin/tezos_actions_1commit.repr --ncommits-trace 12000 --artefacts ./cb_artefacts 1>&2
	@dune exec -- ./bench/irmin-pack/trace_stats.exe cb ./cb_artefacts/stat_summary.json
	@rm -rf ./cb_artefacts

bench: bench-pack-with-trace-replay

bench-inline:
	@echo "Building inline contents benchmark..."
	@dune build test/irmin-pack/bench_inline/main.exe
	@echo "Running benchmark (this may take a few minutes)..."
	@dune exec test/irmin-pack/bench_inline/main.exe -- run --all \
		--contents 5000 --reads 500 --runs 3 \
		> test/irmin-pack/bench_inline/results.csv
	@echo "Generating data files..."
	@cd test/irmin-pack/bench_inline && bash generate_data.sh results.csv
	@echo "Generating plots..."
	@cd test/irmin-pack/bench_inline && gnuplot bench_inline_plot.gp
	@cd test/irmin-pack/bench_inline && gnuplot bench_inline_improvement.gp
	@cd test/irmin-pack/bench_inline && gnuplot bench_inline_storage.gp
	@echo "Done. Results in test/irmin-pack/bench_inline/"
	@echo "  - results.csv: Raw benchmark data"
	@echo "  - bench_inline_comparison.png: Latency comparison"
	@echo "  - bench_inline_improvement.png: Latency improvement"
	@echo "  - bench_inline_storage.png: Storage impact"

fuzz:
	dune build @fuzz --no-buffer

examples:
	dune build @examples

clean:
	dune clean

doc:
	dune build @doc
