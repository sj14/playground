#!/usr/bin/env bash
# Prints how much data the LAN port received per second of softirq time, from the
# RAM log of a test image. It varies less between runs than a single download.
# Usage: cpu-cost.sh <RAM log>
LOG=$1
bytes=$(grep -E '^\[.*\] +lan0:' "$LOG" | tail -1 | sed -E 's/^.*lan0: *//' | awk '{ print $1 }')
first=$(grep -E '^\[.*\] cpu  [0-9]' "$LOG" | head -1 | awk '{ print $10 }')
last=$(grep -E '^\[.*\] cpu  [0-9]' "$LOG" | tail -1 | awk '{ print $10 }')
awk -v bytes="$bytes" -v first="$first" -v last="$last" 'BEGIN {
    megabytes = bytes / 1e6
    seconds = (last - first) / 100
    printf "%.0f MB in %.1f s of softirq time: %.2f MB/s, enough for %.0f Mbit/s\n",
        megabytes, seconds, megabytes / seconds, 8 * megabytes / seconds
}'
