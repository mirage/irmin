#!/bin/sh
# List files in the output directory with their sizes
# Usage: ./list_files.sh <output_dir> [before_snapshot_file]
#
# If before_snapshot_file is provided, only show files that are NEW
# (not in the snapshot). Use with snapshot_files.sh to capture before state.

set -e

if [ -z "$1" ]; then
    echo "Usage: $0 <output_dir> [before_snapshot_file]"
    exit 1
fi

OUTPUT_DIR="$1"
BEFORE_SNAPSHOT="$2"

if [ ! -d "$OUTPUT_DIR" ]; then
    echo "  (no files yet)"
    exit 0
fi

# Get current files
current_files=$(find "$OUTPUT_DIR" -type f ! -name "summary.txt" ! -name ".snapshot_*" 2>/dev/null | sort)

if [ -z "$current_files" ]; then
    echo "  (no output files)"
    exit 0
fi

# If we have a before snapshot, filter to only new files
if [ -n "$BEFORE_SNAPSHOT" ] && [ -f "$BEFORE_SNAPSHOT" ]; then
    new_files=$(echo "$current_files" | while read -r file; do
        if ! grep -qxF "$file" "$BEFORE_SNAPSHOT" 2>/dev/null; then
            echo "$file"
        fi
    done)

    if [ -z "$new_files" ]; then
        echo "  (no new files)"
        exit 0
    fi
    files_to_show="$new_files"
else
    files_to_show="$current_files"
fi

# Display files with sizes
echo "$files_to_show" | while read -r file; do
    [ -z "$file" ] && continue
    fname=$(basename "$file")
    size=$(du -h "$file" 2>/dev/null | cut -f1 || echo "?")
    [ -z "$size" ] && size="0"
    echo "  $fname ($size)"
done
