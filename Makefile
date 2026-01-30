.PHONY: all clean test fuzz bench bench-fast bench-full doc examples \
	docker docker-build docker-tag docker-push \
	bench-mem-fast bench-mem-full \
	bench-hashset-fast bench-hashset-full \
	bench-pack-fast bench-pack-full \
	bench-trace-replay-fast bench-trace-replay-full \
	bench-multicore-half-fast bench-multicore-half-full \
	bench-multicore-full-fast bench-multicore-full-full \
	bench-inline-fast bench-inline-full \
	bench-energy-fast bench-energy-full

all:
	dune build

test:
	dune runtest

# Trace file locations
CI_TRACE_FILE = /home/opam/bench-dir/current-bench-data/mirage/irmin/tezos_actions_1commit.repr
LOCAL_TRACE_FILE = test/irmin-bench/data/tezos_actions_1commit.repr

# UUID generation helper (used by all benchmark targets)
GENERATE_UUID = $$(uuidgen 2>/dev/null || cat /proc/sys/kernel/random/uuid 2>/dev/null || date +%Y%m%d-%H%M%S-$$$$)

# CI benchmark - trace replay with current-bench output format (legacy target)
bench: bench-trace-replay-full

# Composite targets - generate UUID once, run all benchmarks in same directory
bench-fast:
	@export IRMIN_BENCH_ROOT="_metrics/$(GENERATE_UUID)"; \
	mkdir -p "$$IRMIN_BENCH_ROOT"; \
	./write_summary.sh "$$IRMIN_BENCH_ROOT" \
		bench-mem-fast bench-hashset-fast bench-pack-fast bench-trace-replay-fast \
		bench-multicore-half-fast bench-multicore-full-fast bench-inline-fast bench-energy-fast; \
	echo "Benchmark results directory: $$IRMIN_BENCH_ROOT"; \
	echo ""; \
	echo "=== Running bench-mem-fast ==="; \
	SNAP=$$(./snapshot_files.sh "$$IRMIN_BENCH_ROOT"); \
	dune exec -- test/irmin-mem/bench.exe -- fast "$$IRMIN_BENCH_ROOT"; \
	echo "Files created:"; ./list_files.sh "$$IRMIN_BENCH_ROOT" "$$SNAP"; rm -f "$$SNAP"; \
	echo ""; \
	echo "=== Running bench-hashset-fast ==="; \
	SNAP=$$(./snapshot_files.sh "$$IRMIN_BENCH_ROOT"); \
	dune exec -- bench/irmin/data/bench_fixed_size_string_set.exe -- fast "$$IRMIN_BENCH_ROOT"; \
	echo "Files created:"; ./list_files.sh "$$IRMIN_BENCH_ROOT" "$$SNAP"; rm -f "$$SNAP"; \
	echo ""; \
	echo "=== Running bench-pack-fast ==="; \
	SNAP=$$(./snapshot_files.sh "$$IRMIN_BENCH_ROOT"); \
	dune exec -- bench/irmin-pack/main.exe -- fast "$$IRMIN_BENCH_ROOT"; \
	echo "Files created:"; ./list_files.sh "$$IRMIN_BENCH_ROOT" "$$SNAP"; rm -f "$$SNAP"; \
	echo ""; \
	echo "=== Running bench-trace-replay-fast ==="; \
	SNAP=$$(./snapshot_files.sh "$$IRMIN_BENCH_ROOT"); \
	if [ -f "$(LOCAL_TRACE_FILE)" ]; then \
		rm -rf ./cb_artefacts; \
		dune exec -- ./bench/irmin-pack/tree.exe --mode trace $(LOCAL_TRACE_FILE) --ncommits-trace 12000 --artefacts ./cb_artefacts 1>&2; \
		dune exec -- ./bench/irmin-pack/trace_stats.exe cb ./cb_artefacts/stat_summary.json > "$$IRMIN_BENCH_ROOT/trace_replay.json"; \
		rm -rf ./cb_artefacts; \
		echo "Files created:"; ./list_files.sh "$$IRMIN_BENCH_ROOT" "$$SNAP"; \
	else \
		echo "Warning: Local trace file not found: $(LOCAL_TRACE_FILE), skipping trace replay"; \
	fi; \
	rm -f "$$SNAP"; \
	echo ""; \
	echo "=== Running bench-multicore-half-fast ==="; \
	SNAP=$$(./snapshot_files.sh "$$IRMIN_BENCH_ROOT"); \
	dune exec -- test/irmin-pack/bench_multicore/main.exe half --elements 50000 --tasks 1000 --runs 3; \
	echo "Files created:"; ./list_files.sh "$$IRMIN_BENCH_ROOT" "$$SNAP"; rm -f "$$SNAP"; \
	echo ""; \
	echo "=== Running bench-multicore-full-fast ==="; \
	SNAP=$$(./snapshot_files.sh "$$IRMIN_BENCH_ROOT"); \
	dune exec -- test/irmin-pack/bench_multicore/main.exe full --elements 50000 --tasks 1000 --runs 3; \
	echo "Files created:"; ./list_files.sh "$$IRMIN_BENCH_ROOT" "$$SNAP"; rm -f "$$SNAP"; \
	echo ""; \
	echo "=== Running bench-inline-fast ==="; \
	SNAP=$$(./snapshot_files.sh "$$IRMIN_BENCH_ROOT"); \
	dune exec -- test/irmin-pack/bench_inline/main.exe run --all \
		--contents 1000 --reads 100 --runs 2 \
		> "$$IRMIN_BENCH_ROOT/bench_inline.csv"; \
	test/irmin-pack/bench_inline/generate_plots.sh "$$IRMIN_BENCH_ROOT/bench_inline.csv" "$$IRMIN_BENCH_ROOT" 2>/dev/null || \
		echo "  (gnuplot not available for inline benchmark plots)"; \
	echo "Files created:"; ./list_files.sh "$$IRMIN_BENCH_ROOT" "$$SNAP"; rm -f "$$SNAP"; \
	echo ""; \
	echo "=== Running bench-energy-fast ==="; \
	SNAP=$$(./snapshot_files.sh "$$IRMIN_BENCH_ROOT"); \
	ENERGY_OUT="$$IRMIN_BENCH_ROOT/energy.csv"; \
	BENCH_ROOT_SAVE="$$IRMIN_BENCH_ROOT"; \
	unset IRMIN_BENCH_ROOT; \
	cd test/irmin-pack/bench_multicore && ./bench.sh -e 100 -f 100 -r 5 > "$$OLDPWD/$$ENERGY_OUT" && cd "$$OLDPWD"; \
	gnuplot -e "datafile='$$ENERGY_OUT'; outfile='$${ENERGY_OUT%.csv}.png'" test/irmin-pack/bench_multicore/bench.gp 2>/dev/null || echo "  (gnuplot not available or energy data incomplete)"; \
	echo "Files created:"; ./list_files.sh "$$BENCH_ROOT_SAVE" "$$SNAP"; rm -f "$$SNAP"; \
	echo ""; \
	echo "=== All fast benchmarks complete ==="; \
	./finalize_summary.sh "$$BENCH_ROOT_SAVE"; \
	echo ""; \
	echo "Results directory: $$BENCH_ROOT_SAVE"; \
	echo "All generated files:"; ./list_files.sh "$$BENCH_ROOT_SAVE"

bench-full:
	@export IRMIN_BENCH_ROOT="_metrics/$(GENERATE_UUID)"; \
	mkdir -p "$$IRMIN_BENCH_ROOT"; \
	./write_summary.sh "$$IRMIN_BENCH_ROOT" \
		bench-mem-full bench-hashset-full bench-pack-full bench-trace-replay-full \
		bench-multicore-half-full bench-multicore-full-full bench-inline-full bench-energy-full; \
	echo "Benchmark results directory: $$IRMIN_BENCH_ROOT"; \
	echo ""; \
	echo "=== Running bench-mem-full ==="; \
	SNAP=$$(./snapshot_files.sh "$$IRMIN_BENCH_ROOT"); \
	dune exec -- test/irmin-mem/bench.exe -- full "$$IRMIN_BENCH_ROOT"; \
	echo "Files created:"; ./list_files.sh "$$IRMIN_BENCH_ROOT" "$$SNAP"; rm -f "$$SNAP"; \
	echo ""; \
	echo "=== Running bench-hashset-full ==="; \
	SNAP=$$(./snapshot_files.sh "$$IRMIN_BENCH_ROOT"); \
	dune exec -- bench/irmin/data/bench_fixed_size_string_set.exe -- full "$$IRMIN_BENCH_ROOT"; \
	echo "Files created:"; ./list_files.sh "$$IRMIN_BENCH_ROOT" "$$SNAP"; rm -f "$$SNAP"; \
	echo ""; \
	echo "=== Running bench-pack-full ==="; \
	SNAP=$$(./snapshot_files.sh "$$IRMIN_BENCH_ROOT"); \
	dune exec -- bench/irmin-pack/main.exe -- full "$$IRMIN_BENCH_ROOT"; \
	echo "Files created:"; ./list_files.sh "$$IRMIN_BENCH_ROOT" "$$SNAP"; rm -f "$$SNAP"; \
	echo ""; \
	echo "=== Running bench-trace-replay-full ==="; \
	SNAP=$$(./snapshot_files.sh "$$IRMIN_BENCH_ROOT"); \
	if [ -f "$(CI_TRACE_FILE)" ]; then \
		TRACE="$(CI_TRACE_FILE)"; \
	elif [ -f "$(LOCAL_TRACE_FILE)" ]; then \
		TRACE="$(LOCAL_TRACE_FILE)"; \
	else \
		echo "Warning: No trace file found, skipping trace replay"; \
		TRACE=""; \
	fi; \
	if [ -n "$$TRACE" ]; then \
		rm -rf ./cb_artefacts; \
		dune exec -- ./bench/irmin-pack/tree.exe --mode trace $$TRACE --ncommits-trace 12000 --artefacts ./cb_artefacts 1>&2; \
		dune exec -- ./bench/irmin-pack/trace_stats.exe cb ./cb_artefacts/stat_summary.json > "$$IRMIN_BENCH_ROOT/trace_replay.json"; \
		rm -rf ./cb_artefacts; \
		echo "Files created:"; ./list_files.sh "$$IRMIN_BENCH_ROOT" "$$SNAP"; \
	fi; \
	rm -f "$$SNAP"; \
	echo ""; \
	echo "=== Running bench-multicore-half-full ==="; \
	SNAP=$$(./snapshot_files.sh "$$IRMIN_BENCH_ROOT"); \
	dune exec -- test/irmin-pack/bench_multicore/main.exe half; \
	echo "Files created:"; ./list_files.sh "$$IRMIN_BENCH_ROOT" "$$SNAP"; rm -f "$$SNAP"; \
	echo ""; \
	echo "=== Running bench-multicore-full-full ==="; \
	SNAP=$$(./snapshot_files.sh "$$IRMIN_BENCH_ROOT"); \
	dune exec -- test/irmin-pack/bench_multicore/main.exe full; \
	echo "Files created:"; ./list_files.sh "$$IRMIN_BENCH_ROOT" "$$SNAP"; rm -f "$$SNAP"; \
	echo ""; \
	echo "=== Running bench-inline-full ==="; \
	SNAP=$$(./snapshot_files.sh "$$IRMIN_BENCH_ROOT"); \
	dune exec -- test/irmin-pack/bench_inline/main.exe run --all \
		--contents 10000 --reads 1000 --runs 5 \
		> "$$IRMIN_BENCH_ROOT/bench_inline.csv"; \
	test/irmin-pack/bench_inline/generate_plots.sh "$$IRMIN_BENCH_ROOT/bench_inline.csv" "$$IRMIN_BENCH_ROOT" 2>/dev/null || \
		echo "  (gnuplot not available for inline benchmark plots)"; \
	echo "Files created:"; ./list_files.sh "$$IRMIN_BENCH_ROOT" "$$SNAP"; rm -f "$$SNAP"; \
	echo ""; \
	echo "=== Running bench-energy-full ==="; \
	SNAP=$$(./snapshot_files.sh "$$IRMIN_BENCH_ROOT"); \
	ENERGY_OUT="$$IRMIN_BENCH_ROOT/energy.csv"; \
	BENCH_ROOT_SAVE="$$IRMIN_BENCH_ROOT"; \
	unset IRMIN_BENCH_ROOT; \
	cd test/irmin-pack/bench_multicore && ./bench.sh -e 1000 -f 1000 -r 10 > "$$OLDPWD/$$ENERGY_OUT" && cd "$$OLDPWD"; \
	gnuplot -e "datafile='$$ENERGY_OUT'; outfile='$${ENERGY_OUT%.csv}.png'" test/irmin-pack/bench_multicore/bench.gp 2>/dev/null || echo "  (gnuplot not available or energy data incomplete)"; \
	echo "Files created:"; ./list_files.sh "$$BENCH_ROOT_SAVE" "$$SNAP"; rm -f "$$SNAP"; \
	echo ""; \
	echo "=== All full benchmarks complete ==="; \
	./finalize_summary.sh "$$BENCH_ROOT_SAVE"; \
	echo ""; \
	echo "Results directory: $$BENCH_ROOT_SAVE"; \
	echo "All generated files:"; ./list_files.sh "$$BENCH_ROOT_SAVE"

# Individual benchmark targets - generate own UUID if IRMIN_BENCH_ROOT not set

# In-memory store benchmarks
bench-mem-fast:
	@if [ -z "$$IRMIN_BENCH_ROOT" ]; then \
		export IRMIN_BENCH_ROOT="_metrics/$(GENERATE_UUID)"; \
		mkdir -p "$$IRMIN_BENCH_ROOT"; \
		./write_summary.sh "$$IRMIN_BENCH_ROOT" bench-mem-fast; \
	fi; \
	dune exec -- test/irmin-mem/bench.exe -- fast "$$IRMIN_BENCH_ROOT"

bench-mem-full:
	@if [ -z "$$IRMIN_BENCH_ROOT" ]; then \
		export IRMIN_BENCH_ROOT="_metrics/$(GENERATE_UUID)"; \
		mkdir -p "$$IRMIN_BENCH_ROOT"; \
		./write_summary.sh "$$IRMIN_BENCH_ROOT" bench-mem-full; \
	fi; \
	dune exec -- test/irmin-mem/bench.exe -- full "$$IRMIN_BENCH_ROOT"

# Fixed-size string hashset benchmarks
bench-hashset-fast:
	@if [ -z "$$IRMIN_BENCH_ROOT" ]; then \
		export IRMIN_BENCH_ROOT="_metrics/$(GENERATE_UUID)"; \
		mkdir -p "$$IRMIN_BENCH_ROOT"; \
		./write_summary.sh "$$IRMIN_BENCH_ROOT" bench-hashset-fast; \
	fi; \
	dune exec -- bench/irmin/data/bench_fixed_size_string_set.exe -- fast "$$IRMIN_BENCH_ROOT"

bench-hashset-full:
	@if [ -z "$$IRMIN_BENCH_ROOT" ]; then \
		export IRMIN_BENCH_ROOT="_metrics/$(GENERATE_UUID)"; \
		mkdir -p "$$IRMIN_BENCH_ROOT"; \
		./write_summary.sh "$$IRMIN_BENCH_ROOT" bench-hashset-full; \
	fi; \
	dune exec -- bench/irmin/data/bench_fixed_size_string_set.exe -- full "$$IRMIN_BENCH_ROOT"

# irmin-pack store benchmarks
bench-pack-fast:
	@if [ -z "$$IRMIN_BENCH_ROOT" ]; then \
		export IRMIN_BENCH_ROOT="_metrics/$(GENERATE_UUID)"; \
		mkdir -p "$$IRMIN_BENCH_ROOT"; \
		./write_summary.sh "$$IRMIN_BENCH_ROOT" bench-pack-fast; \
	fi; \
	dune exec -- bench/irmin-pack/main.exe -- fast "$$IRMIN_BENCH_ROOT"

bench-pack-full:
	@if [ -z "$$IRMIN_BENCH_ROOT" ]; then \
		export IRMIN_BENCH_ROOT="_metrics/$(GENERATE_UUID)"; \
		mkdir -p "$$IRMIN_BENCH_ROOT"; \
		./write_summary.sh "$$IRMIN_BENCH_ROOT" bench-pack-full; \
	fi; \
	dune exec -- bench/irmin-pack/main.exe -- full "$$IRMIN_BENCH_ROOT"

# Trace replay benchmarks
bench-trace-replay-fast:
	@if [ ! -f "$(LOCAL_TRACE_FILE)" ]; then \
		echo "Error: Local trace file not found: $(LOCAL_TRACE_FILE)"; \
		exit 1; \
	fi; \
	if [ -z "$$IRMIN_BENCH_ROOT" ]; then \
		export IRMIN_BENCH_ROOT="_metrics/$(GENERATE_UUID)"; \
		mkdir -p "$$IRMIN_BENCH_ROOT"; \
		./write_summary.sh "$$IRMIN_BENCH_ROOT" bench-trace-replay-fast; \
	fi; \
	rm -rf ./cb_artefacts; \
	dune exec -- ./bench/irmin-pack/tree.exe --mode trace $(LOCAL_TRACE_FILE) --ncommits-trace 12000 --artefacts ./cb_artefacts 1>&2; \
	dune exec -- ./bench/irmin-pack/trace_stats.exe cb ./cb_artefacts/stat_summary.json > "$$IRMIN_BENCH_ROOT/trace_replay.json"; \
	rm -rf ./cb_artefacts; \
	echo "Results: $$(cd "$$IRMIN_BENCH_ROOT" && pwd)"

bench-trace-replay-full:
	@if [ -f "$(CI_TRACE_FILE)" ]; then \
		TRACE="$(CI_TRACE_FILE)"; \
	elif [ -f "$(LOCAL_TRACE_FILE)" ]; then \
		TRACE="$(LOCAL_TRACE_FILE)"; \
	else \
		echo "Error: No trace file found"; \
		exit 1; \
	fi; \
	if [ -z "$$IRMIN_BENCH_ROOT" ]; then \
		export IRMIN_BENCH_ROOT="_metrics/$(GENERATE_UUID)"; \
		mkdir -p "$$IRMIN_BENCH_ROOT"; \
		./write_summary.sh "$$IRMIN_BENCH_ROOT" bench-trace-replay-full; \
	fi; \
	rm -rf ./cb_artefacts; \
	dune exec -- ./bench/irmin-pack/tree.exe --mode trace $$TRACE --ncommits-trace 12000 --artefacts ./cb_artefacts 1>&2; \
	dune exec -- ./bench/irmin-pack/trace_stats.exe cb ./cb_artefacts/stat_summary.json > "$$IRMIN_BENCH_ROOT/trace_replay.json"; \
	rm -rf ./cb_artefacts; \
	echo "Results: $$(cd "$$IRMIN_BENCH_ROOT" && pwd)"

# Multicore benchmarks - half-diamond shape
bench-multicore-half-fast:
	@if [ -z "$$IRMIN_BENCH_ROOT" ]; then \
		export IRMIN_BENCH_ROOT="_metrics/$(GENERATE_UUID)"; \
		mkdir -p "$$IRMIN_BENCH_ROOT"; \
		./write_summary.sh "$$IRMIN_BENCH_ROOT" bench-multicore-half-fast; \
	fi; \
	dune exec -- test/irmin-pack/bench_multicore/main.exe half --elements 50000 --tasks 1000 --runs 3

bench-multicore-half-full:
	@if [ -z "$$IRMIN_BENCH_ROOT" ]; then \
		export IRMIN_BENCH_ROOT="_metrics/$(GENERATE_UUID)"; \
		mkdir -p "$$IRMIN_BENCH_ROOT"; \
		./write_summary.sh "$$IRMIN_BENCH_ROOT" bench-multicore-half-full; \
	fi; \
	dune exec -- test/irmin-pack/bench_multicore/main.exe half

# Multicore benchmarks - full-diamond shape
bench-multicore-full-fast:
	@if [ -z "$$IRMIN_BENCH_ROOT" ]; then \
		export IRMIN_BENCH_ROOT="_metrics/$(GENERATE_UUID)"; \
		mkdir -p "$$IRMIN_BENCH_ROOT"; \
		./write_summary.sh "$$IRMIN_BENCH_ROOT" bench-multicore-full-fast; \
	fi; \
	dune exec -- test/irmin-pack/bench_multicore/main.exe full --elements 50000 --tasks 1000 --runs 3

bench-multicore-full-full:
	@if [ -z "$$IRMIN_BENCH_ROOT" ]; then \
		export IRMIN_BENCH_ROOT="_metrics/$(GENERATE_UUID)"; \
		mkdir -p "$$IRMIN_BENCH_ROOT"; \
		./write_summary.sh "$$IRMIN_BENCH_ROOT" bench-multicore-full-full; \
	fi; \
	dune exec -- test/irmin-pack/bench_multicore/main.exe full

# Inline contents benchmarks
bench-inline-fast:
	@if [ -z "$$IRMIN_BENCH_ROOT" ]; then \
		export IRMIN_BENCH_ROOT="_metrics/$(GENERATE_UUID)"; \
		mkdir -p "$$IRMIN_BENCH_ROOT"; \
		./write_summary.sh "$$IRMIN_BENCH_ROOT" bench-inline-fast; \
	fi; \
	dune exec -- test/irmin-pack/bench_inline/main.exe run --all \
		--contents 1000 --reads 100 --runs 2 \
		> "$$IRMIN_BENCH_ROOT/bench_inline.csv"; \
	test/irmin-pack/bench_inline/generate_plots.sh "$$IRMIN_BENCH_ROOT/bench_inline.csv" "$$IRMIN_BENCH_ROOT" 2>/dev/null || \
		echo "  (gnuplot not available for inline benchmark plots)"; \
	echo "Results: $$(cd "$$IRMIN_BENCH_ROOT" && pwd)"

bench-inline-full:
	@if [ -z "$$IRMIN_BENCH_ROOT" ]; then \
		export IRMIN_BENCH_ROOT="_metrics/$(GENERATE_UUID)"; \
		mkdir -p "$$IRMIN_BENCH_ROOT"; \
		./write_summary.sh "$$IRMIN_BENCH_ROOT" bench-inline-full; \
	fi; \
	dune exec -- test/irmin-pack/bench_inline/main.exe run --all \
		--contents 10000 --reads 1000 --runs 5 \
		> "$$IRMIN_BENCH_ROOT/bench_inline.csv"; \
	test/irmin-pack/bench_inline/generate_plots.sh "$$IRMIN_BENCH_ROOT/bench_inline.csv" "$$IRMIN_BENCH_ROOT" 2>/dev/null || \
		echo "  (gnuplot not available for inline benchmark plots)"; \
	echo "Results: $$(cd "$$IRMIN_BENCH_ROOT" && pwd)"

# Energy consumption benchmarks (requires perf with energy support)
bench-energy-fast:
	@if [ -z "$$IRMIN_BENCH_ROOT" ]; then \
		IRMIN_BENCH_ROOT="_metrics/$(GENERATE_UUID)"; \
		mkdir -p "$$IRMIN_BENCH_ROOT"; \
		./write_summary.sh "$$IRMIN_BENCH_ROOT" bench-energy-fast; \
	fi; \
	ENERGY_OUT="$$IRMIN_BENCH_ROOT/energy.csv"; \
	unset IRMIN_BENCH_ROOT; \
	cd test/irmin-pack/bench_multicore && ./bench.sh -e 100 -f 100 -r 5 > "$$OLDPWD/$$ENERGY_OUT" && cd "$$OLDPWD"; \
	gnuplot -e "datafile='$$ENERGY_OUT'; outfile='$${ENERGY_OUT%.csv}.png'" test/irmin-pack/bench_multicore/bench.gp 2>/dev/null || echo "  (gnuplot not available or energy data incomplete)"; \
	echo "Results: $$(cd "$$(dirname $$ENERGY_OUT)" && pwd)"

bench-energy-full:
	@if [ -z "$$IRMIN_BENCH_ROOT" ]; then \
		IRMIN_BENCH_ROOT="_metrics/$(GENERATE_UUID)"; \
		mkdir -p "$$IRMIN_BENCH_ROOT"; \
		./write_summary.sh "$$IRMIN_BENCH_ROOT" bench-energy-full; \
	fi; \
	ENERGY_OUT="$$IRMIN_BENCH_ROOT/energy.csv"; \
	unset IRMIN_BENCH_ROOT; \
	cd test/irmin-pack/bench_multicore && ./bench.sh -e 1000 -f 1000 -r 10 > "$$OLDPWD/$$ENERGY_OUT" && cd "$$OLDPWD"; \
	gnuplot -e "datafile='$$ENERGY_OUT'; outfile='$${ENERGY_OUT%.csv}.png'" test/irmin-pack/bench_multicore/bench.gp 2>/dev/null || echo "  (gnuplot not available or energy data incomplete)"; \
	echo "Results: $$(cd "$$(dirname $$ENERGY_OUT)" && pwd)"

fuzz:
	dune build @fuzz --no-buffer

examples:
	dune build @examples

clean:
	dune clean

doc:
	dune build @doc

# Docker targets
DOCKER_USER = trustchainspice
DOCKER_IMAGE = spice

docker: docker-build

docker-build:
	docker build -t $(DOCKER_IMAGE) .

docker-tag: docker-build
	@echo "Tagging image for Docker Hub user: $(DOCKER_USER)"
	@echo "Make sure you are logged in as $(DOCKER_USER) (docker login)"
	docker tag $(DOCKER_IMAGE) $(DOCKER_USER)/$(DOCKER_IMAGE):latest

docker-push: docker-tag
	@echo "Pushing to Docker Hub as $(DOCKER_USER)"
	@echo "Ensure you have run: docker login -u $(DOCKER_USER)"
	docker push $(DOCKER_USER)/$(DOCKER_IMAGE):latest
