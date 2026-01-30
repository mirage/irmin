#!/bin/sh
# Finalize the summary file by listing all generated files
# Usage: ./finalize_summary.sh <output_dir>

set -e

if [ -z "$1" ]; then
    echo "Usage: $0 <output_dir>"
    exit 1
fi

OUTPUT_DIR="$1"
SUMMARY_FILE="$OUTPUT_DIR/summary.txt"

# List all files in the output directory (excluding summary.txt itself)
# and append to the Generated Files section
{
    find "$OUTPUT_DIR" -type f ! -name "summary.txt" | sort | while read -r file; do
        # Get relative path from OUTPUT_DIR
        relpath="${file#$OUTPUT_DIR/}"
        size=$(du -h "$file" 2>/dev/null | cut -f1)
        echo "- $relpath ($size)"
    done
} >> "$SUMMARY_FILE"

echo "" >> "$SUMMARY_FILE"
