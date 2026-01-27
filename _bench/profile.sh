#!/bin/bash
# Profiling script for irmin-pack benchmarks
# Usage: ./_bench/profile.sh [output_dir]
#
# This script can be used on any branch to collect comparable profiling data.
# Results are saved to the specified directory (default: _bench_results/<branch>-<timestamp>)

set -e

# Configuration
BENCH_EXE="_build/default/bench/irmin-pack/main.exe"
# For timing: quick runs
BENCH_ARGS_TIMING="-n 10 -d 5 -a 100"
# For profiling: longer run so actual work dominates over gnuplot
BENCH_ARGS_PROFILE="-n 100 -d 10 -a 100"
export EIO_BACKEND=posix

# Determine output directory
BRANCH=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")
TIMESTAMP=$(date +%Y%m%d-%H%M%S)
COMMIT=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
OUTPUT_DIR="${1:-_bench_results/${BRANCH}-${TIMESTAMP}}"

mkdir -p "$OUTPUT_DIR"

echo "=== Irmin-pack Profiling ==="
echo "Branch: $BRANCH"
echo "Commit: $COMMIT"
echo "Output: $OUTPUT_DIR"
echo ""

# Save metadata
cat > "$OUTPUT_DIR/metadata.txt" << EOF
branch: $BRANCH
commit: $COMMIT
timestamp: $TIMESTAMP
bench_args_timing: $BENCH_ARGS_TIMING
bench_args_profile: $BENCH_ARGS_PROFILE
EOF

# Build with debug info
echo "=== Building with debug symbols ==="
dune build bench/irmin-pack/main.exe

# Run 1: Basic timing (multiple runs for variance)
echo ""
echo "=== Timing runs (3x) ==="
for i in 1 2 3; do
    BENCH_ROOT=$(mktemp -d)
    IRMIN_BENCH_ROOT="$BENCH_ROOT" "$BENCH_EXE" $BENCH_ARGS_TIMING 2>&1
    cat "$BENCH_ROOT/metrics/bench.data" >> "$OUTPUT_DIR/timing.txt"
    rm -rf "$BENCH_ROOT"
done
echo "Timing results:"
cat "$OUTPUT_DIR/timing.txt"

# Run 2: perf record (longer run to get meaningful samples)
echo ""
echo "=== Perf profiling (longer run: $BENCH_ARGS_PROFILE) ==="
BENCH_ROOT=$(mktemp -d)
if command -v perf &> /dev/null; then
    perf record -g --call-graph dwarf -o "$OUTPUT_DIR/perf.data" -- \
        env IRMIN_BENCH_ROOT="$BENCH_ROOT" "$BENCH_EXE" $BENCH_ARGS_PROFILE 2>&1

    # Generate text report
    perf report -i "$OUTPUT_DIR/perf.data" --stdio --no-children > "$OUTPUT_DIR/perf-report.txt" 2>&1 || true

    # Top functions
    perf report -i "$OUTPUT_DIR/perf.data" --stdio --no-children -n --percent-limit 1 > "$OUTPUT_DIR/perf-top.txt" 2>&1 || true

    echo "Perf data saved to $OUTPUT_DIR/perf.data"
    echo "Top functions:"
    head -50 "$OUTPUT_DIR/perf-top.txt" 2>/dev/null || echo "(perf report failed)"
else
    echo "perf not available, skipping"
fi
rm -rf "$BENCH_ROOT"

# Run 3: GC stats
echo ""
echo "=== GC statistics ==="
BENCH_ROOT=$(mktemp -d)
OCAMLRUNPARAM="v=0x01" IRMIN_BENCH_ROOT="$BENCH_ROOT" "$BENCH_EXE" $BENCH_ARGS_TIMING 2> "$OUTPUT_DIR/gc-stats.txt" || true
rm -rf "$BENCH_ROOT"
if [ -s "$OUTPUT_DIR/gc-stats.txt" ]; then
    echo "GC stats saved to $OUTPUT_DIR/gc-stats.txt"
    tail -20 "$OUTPUT_DIR/gc-stats.txt"
else
    echo "No GC stats collected"
fi

echo ""
echo "=== Done ==="
echo "Results saved to: $OUTPUT_DIR"
echo ""
echo "To compare with another branch:"
echo "  1. git checkout <other-branch>"
echo "  2. ./_bench/profile.sh"
echo "  3. diff -u $OUTPUT_DIR/perf-top.txt _bench_results/<other>/perf-top.txt"
