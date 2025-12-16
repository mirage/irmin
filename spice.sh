#!/bin/sh

echo "Building irmin..."    
dune build
echo "Building irmin done."

echo "Running tests..."    
dune runtest
echo "Running tests done."

echo "Running benchmarks..."    
dune exec -- test/irmin-mem/bench.exe
#dune exec -- test/irmin-pack/bench_multicore/main.exe half
#dune exec -- test/irmin-pack/bench_multicore/main.exe full
dune exec -- bench/irmin/data/bench_fixed_size_string_set.exe
dune exec -- bench/irmin-pack/main.exe
dune exec -- bench/irmin-pack/trace_stats.exe
#dune exec -- bench/irmin-pack/tree.exe

echo "Running benchmarks done."
