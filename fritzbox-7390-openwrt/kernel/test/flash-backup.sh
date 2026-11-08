#!/usr/bin/env bash
# Copies a flash partition from the box under Freetz, read only, in pieces of
# 32 KiB, because the shell page cuts its output after 64,000 bytes. Use the read
# only character devices: /dev/mtdblock1 drops the last 256 bytes of the kernel
# partition, whose size is not a multiple of 512.
# Usage: flash-backup.sh <mtd number> <size in bytes> <output file>
set -u
TEST_DIR=$(cd "$(dirname "$0")" && pwd)
. "$TEST_DIR/freetz.sh"
PARTITION=$1
SIZE=$2
OUT=$3
PIECE=32768
SCRIPT=$(mktemp)
trap 'rm -f "$SCRIPT"' EXIT

: > "$OUT"
pieces=$(( (SIZE + PIECE - 1) / PIECE ))
for piece in $(seq 0 $((pieces - 1))); do
    expected=$(( piece == pieces - 1 ? SIZE - piece * PIECE : PIECE ))
    printf 'echo PIECE-BEGIN\ndd if=/dev/mtd%sro bs=%s skip=%s count=1 2>/dev/null | base64\necho PIECE-END\n' \
        "$PARTITION" "$PIECE" "$piece" > "$SCRIPT"
    for attempt in 1 2 3; do
        data=$(freetz_run "$SCRIPT" | sed -n '/^PIECE-BEGIN$/,/^PIECE-END$/p' | sed '1d;$d' | base64 -d 2> /dev/null | base64 -w0)
        if [ -n "$data" ] && [ "$(printf '%s' "$data" | base64 -d | wc -c)" = "$expected" ]; then
            printf '%s' "$data" | base64 -d >> "$OUT"
            continue 2
        fi
    done
    echo "piece $piece failed"
    exit 1
done

printf 'md5sum /dev/mtd%sro\n' "$PARTITION" > "$SCRIPT"
echo "box:  $(freetz_run "$SCRIPT" | awk '{ print $1 }')"
echo "copy: $(md5sum < "$OUT" | awk '{ print $1 }')"
