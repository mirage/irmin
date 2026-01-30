#!/bin/sh
# Generate gnuplot diagrams from inline benchmark CSV output
# Usage: ./generate_plots.sh <input.csv> <output_dir>

set -e

if [ $# -lt 2 ]; then
    echo "Usage: $0 <input.csv> <output_dir>"
    exit 1
fi

INPUT_CSV="$1"
OUTPUT_DIR="$2"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

mkdir -p "$OUTPUT_DIR"

# Generate bench_inline_plot.dat (latency comparison)
# Format: Distribution    No-Inline    Inline-48    Improvement%
echo "# Distribution    No-Inline    Inline-48    Improvement%" > "$OUTPUT_DIR/bench_inline_plot.dat"

# Process CSV: group by distribution, extract read_p99_us for off and 48 threshold
awk -F, '
NR > 1 {
    dist = $1
    threshold = $2
    read_p99 = $7

    if (threshold == "off") {
        no_inline[dist] = read_p99
    } else if (threshold == "48") {
        inline[dist] = read_p99
    }
}
END {
    for (dist in no_inline) {
        if (dist in inline) {
            improvement = (no_inline[dist] - inline[dist]) / no_inline[dist] * 100
            printf "%-20s %8.2f %12.2f %12.1f\n", dist, no_inline[dist], inline[dist], improvement
        }
    }
}
' "$INPUT_CSV" >> "$OUTPUT_DIR/bench_inline_plot.dat"

# Generate bench_inline_storage.dat (storage impact)
# Format: index  distribution  no_inline_bytes  inline_bytes  change%  inlined_ratio%
echo "# idx  distribution  no_inline_bytes  inline_bytes  change%  inlined_ratio%" > "$OUTPUT_DIR/bench_inline_storage.dat"

awk -F, '
NR > 1 {
    dist = $1
    threshold = $2
    store_bytes = $4
    inlined = $9
    non_inlined = $10

    if (threshold == "off") {
        no_inline_bytes[dist] = store_bytes
    } else if (threshold == "48") {
        inline_bytes[dist] = store_bytes
        inlined_count[dist] = inlined
        non_inlined_count[dist] = non_inlined
    }
}
END {
    idx = 0
    for (dist in no_inline_bytes) {
        if (dist in inline_bytes) {
            change = (inline_bytes[dist] - no_inline_bytes[dist]) / no_inline_bytes[dist] * 100
            total = inlined_count[dist] + non_inlined_count[dist]
            if (total > 0) {
                inlined_ratio = inlined_count[dist] / total * 100
            } else {
                inlined_ratio = 0
            }
            printf "%d  %-20s %d %d %.1f %.1f\n", idx, dist, no_inline_bytes[dist], inline_bytes[dist], change, inlined_ratio
            idx++
        }
    }
}
' "$INPUT_CSV" >> "$OUTPUT_DIR/bench_inline_storage.dat"

# Generate plots with gnuplot
cd "$OUTPUT_DIR"

# Plot 1: Latency comparison (bar chart)
if [ -f "$SCRIPT_DIR/bench_inline_plot.gp" ]; then
    gnuplot -e "datafile='bench_inline_plot.dat'; outfile='bench_inline_comparison.png'" \
        "$SCRIPT_DIR/bench_inline_plot.gp" 2>/dev/null || \
        echo "  (gnuplot failed for comparison plot)"
fi

# Plot 2: Improvement percentage
if [ -f "$SCRIPT_DIR/bench_inline_improvement.gp" ]; then
    gnuplot -e "datafile='bench_inline_plot.dat'; outfile='bench_inline_improvement.png'" \
        "$SCRIPT_DIR/bench_inline_improvement.gp" 2>/dev/null || \
        echo "  (gnuplot failed for improvement plot)"
fi

# Plot 3: Storage impact
if [ -f "$SCRIPT_DIR/bench_inline_storage.gp" ]; then
    gnuplot -e "datafile='bench_inline_storage.dat'; outfile='bench_inline_storage.png'" \
        "$SCRIPT_DIR/bench_inline_storage.gp" 2>/dev/null || \
        echo "  (gnuplot failed for storage plot)"
fi

echo "Plots generated in $OUTPUT_DIR"
