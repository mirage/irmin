#!/bin/bash
# Compare profiling results between two runs
# Usage: ./_bench/compare.sh <dir1> <dir2>

set -e

if [ $# -ne 2 ]; then
    echo "Usage: $0 <results_dir_1> <results_dir_2>"
    echo ""
    echo "Example: $0 _bench_results/main-20240127 _bench_results/benchmarks-20240127"
    exit 1
fi

DIR1="$1"
DIR2="$2"

if [ ! -d "$DIR1" ] || [ ! -d "$DIR2" ]; then
    echo "Error: Both directories must exist"
    exit 1
fi

echo "=== Comparing Profiling Results ==="
echo ""
echo "Directory 1: $DIR1"
cat "$DIR1/metadata.txt" 2>/dev/null | sed 's/^/  /'
echo ""
echo "Directory 2: $DIR2"
cat "$DIR2/metadata.txt" 2>/dev/null | sed 's/^/  /'
echo ""

# Compare timing
echo "=== Timing Comparison ==="
echo ""
echo "[$DIR1]"
if [ -f "$DIR1/timing.txt" ]; then
    awk -F',' '{sum += $1; count++} END {printf "  Mean: %.2f ms (n=%d)\n", sum/count/1000000, count}' "$DIR1/timing.txt"
    awk -F',' '{print $1}' "$DIR1/timing.txt" | sort -n | awk '{a[NR]=$1} END {printf "  Min:  %.2f ms\n  Max:  %.2f ms\n", a[1]/1000000, a[NR]/1000000}'
fi

echo ""
echo "[$DIR2]"
if [ -f "$DIR2/timing.txt" ]; then
    awk -F',' '{sum += $1; count++} END {printf "  Mean: %.2f ms (n=%d)\n", sum/count/1000000, count}' "$DIR2/timing.txt"
    awk -F',' '{print $1}' "$DIR2/timing.txt" | sort -n | awk '{a[NR]=$1} END {printf "  Min:  %.2f ms\n  Max:  %.2f ms\n", a[1]/1000000, a[NR]/1000000}'
fi

# Compare top functions
echo ""
echo "=== Top Functions Comparison ==="
echo ""
echo "Functions with >2% in either profile:"
echo ""

# Extract function names and percentages, compare side by side
if [ -f "$DIR1/perf-top.txt" ] && [ -f "$DIR2/perf-top.txt" ]; then
    # Create temp files with function -> percentage mapping
    grep -E '^\s+[0-9]+\.[0-9]+%' "$DIR1/perf-top.txt" | awk '{pct=$1; $1=""; name=$0; gsub(/^[ \t]+/, "", name); print pct, name}' | head -30 > /tmp/perf1.txt 2>/dev/null || true
    grep -E '^\s+[0-9]+%' "$DIR1/perf-top.txt" | awk '{pct=$1; $1=""; name=$0; gsub(/^[ \t]+/, "", name); print pct, name}' >> /tmp/perf1.txt 2>/dev/null || true

    grep -E '^\s+[0-9]+\.[0-9]+%' "$DIR2/perf-top.txt" | awk '{pct=$1; $1=""; name=$0; gsub(/^[ \t]+/, "", name); print pct, name}' | head -30 > /tmp/perf2.txt 2>/dev/null || true
    grep -E '^\s+[0-9]+%' "$DIR2/perf-top.txt" | awk '{pct=$1; $1=""; name=$0; gsub(/^[ \t]+/, "", name); print pct, name}' >> /tmp/perf2.txt 2>/dev/null || true

    printf "%-40s %10s %10s\n" "Function" "$(basename $DIR1)" "$(basename $DIR2)"
    printf "%-40s %10s %10s\n" "----------------------------------------" "----------" "----------"

    # Simple side-by-side of top 20 from each
    echo ""
    echo "Top from $DIR1:"
    head -15 /tmp/perf1.txt 2>/dev/null || echo "  (no data)"
    echo ""
    echo "Top from $DIR2:"
    head -15 /tmp/perf2.txt 2>/dev/null || echo "  (no data)"

    rm -f /tmp/perf1.txt /tmp/perf2.txt
else
    echo "  (perf data not available in one or both directories)"
fi

echo ""
echo "=== Full diff of perf reports ==="
echo "Run: diff $DIR1/perf-top.txt $DIR2/perf-top.txt"
