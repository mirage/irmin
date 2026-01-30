#!/bin/sh
# Capture a snapshot of files in a directory
# Usage: ./snapshot_files.sh <output_dir>
# Outputs the snapshot filename to stdout

set -e

if [ -z "$1" ]; then
    echo "Usage: $0 <output_dir>" >&2
    exit 1
fi

OUTPUT_DIR="$1"
SNAPSHOT_FILE="$OUTPUT_DIR/.snapshot_$$"

mkdir -p "$OUTPUT_DIR"
find "$OUTPUT_DIR" -type f ! -name "summary.txt" ! -name ".snapshot_*" 2>/dev/null | sort > "$SNAPSHOT_FILE"
echo "$SNAPSHOT_FILE"
