#!/bin/bash
# Generate gnuplot data files from benchmark CSV output
# Usage: ./generate_data.sh results.csv

set -e

INPUT="${1:-results.csv}"

if [ ! -f "$INPUT" ]; then
    echo "Error: Input file '$INPUT' not found"
    exit 1
fi

# Generate latency data (bench_inline_plot.dat)
# Format: Distribution  No-Inline  Inline-48  Improvement%
echo "# Distribution    No-Inline    Inline-48    Improvement%" > bench_inline_plot.dat

# Process pairs of lines (off, then 48) for each distribution
awk -F',' '
NR > 1 {
    dist = $1
    threshold = $2
    p99 = $7

    if (threshold == "off") {
        no_inline[dist] = p99
    } else {
        inline[dist] = p99
        if (no_inline[dist] > 0) {
            improvement = (no_inline[dist] - p99) / no_inline[dist] * 100
            printf "%-18s %8.2f     %8.2f     %5.1f\n", dist, no_inline[dist], p99, improvement
        }
    }
}
' "$INPUT" >> bench_inline_plot.dat

# Generate storage data (bench_inline_storage.dat)
# Format: index, distribution, no_inline_bytes, inline_bytes, pct_change, inlined_ratio
echo "# Storage impact data for inline contents benchmark" > bench_inline_storage.dat
echo "# Columns: index, distribution, no_inline_bytes, inline_bytes, pct_change, inlined_ratio" >> bench_inline_storage.dat

awk -F',' '
NR > 1 {
    dist = $1
    threshold = $2
    store_bytes = $4
    inlined = $9
    non_inlined = $10

    if (threshold == "off") {
        no_inline_bytes[dist] = store_bytes
        order[++n] = dist
    } else {
        inline_bytes[dist] = store_bytes
        inlined_count[dist] = inlined
        non_inlined_count[dist] = non_inlined
    }
}
END {
    for (i = 1; i <= n; i++) {
        dist = order[i]
        nib = no_inline_bytes[dist]
        ib = inline_bytes[dist]
        pct = (ib - nib) / nib * 100
        total = inlined_count[dist] + non_inlined_count[dist]
        ratio = (total > 0) ? inlined_count[dist] / total * 100 : 0
        printf "%d\t%s\t%d\t%d\t%.1f\t%.1f\n", i, dist, nib, ib, pct, ratio
    }
}
' "$INPUT" >> bench_inline_storage.dat

echo "Generated bench_inline_plot.dat and bench_inline_storage.dat"
