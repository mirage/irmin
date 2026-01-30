#!/bin/sh
# Generate a summary file for benchmark runs
# Usage: ./write_summary.sh <output_dir> [benchmark_name...]

set -e

if [ -z "$1" ]; then
    echo "Usage: $0 <output_dir> [benchmark_name...]"
    exit 1
fi

OUTPUT_DIR="$1"
shift
SUMMARY_FILE="$OUTPUT_DIR/summary.txt"

# Create or truncate summary file
cat > "$SUMMARY_FILE" << 'HEADER'
# Benchmark Run Summary

HEADER

# Run Information
echo "## Run Information" >> "$SUMMARY_FILE"
echo "Date: $(date -Iseconds)" >> "$SUMMARY_FILE"
echo "User: $(whoami)" >> "$SUMMARY_FILE"
echo "Hostname: $(hostname)" >> "$SUMMARY_FILE"
echo "Working directory: $(pwd)" >> "$SUMMARY_FILE"
echo "Git branch: $(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo 'N/A')" >> "$SUMMARY_FILE"
echo "Git commit: $(git rev-parse --short HEAD 2>/dev/null || echo 'N/A')" >> "$SUMMARY_FILE"
echo "" >> "$SUMMARY_FILE"

# CPU Information
echo "## CPU Information" >> "$SUMMARY_FILE"
model=$(grep -m1 'model name' /proc/cpuinfo 2>/dev/null | cut -d: -f2 | xargs || echo 'N/A')
echo "Model: $model" >> "$SUMMARY_FILE"
echo "Cores: $(nproc 2>/dev/null || echo 'N/A')" >> "$SUMMARY_FILE"
echo "Architecture: $(uname -m)" >> "$SUMMARY_FILE"
echo "" >> "$SUMMARY_FILE"

# Memory Information
echo "## Memory Information" >> "$SUMMARY_FILE"
if command -v free >/dev/null 2>&1; then
    free -h | head -2 >> "$SUMMARY_FILE"
else
    echo "N/A" >> "$SUMMARY_FILE"
fi
echo "" >> "$SUMMARY_FILE"

# Benchmarks Executed
echo "## Benchmarks Executed" >> "$SUMMARY_FILE"
if [ $# -gt 0 ]; then
    for bench in "$@"; do
        echo "- $bench" >> "$SUMMARY_FILE"
    done
else
    echo "(none specified)" >> "$SUMMARY_FILE"
fi
echo "" >> "$SUMMARY_FILE"

# Placeholder for generated files (will be filled by finalize_summary.sh)
echo "## Generated Files" >> "$SUMMARY_FILE"
echo "" >> "$SUMMARY_FILE"
