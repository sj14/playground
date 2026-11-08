#!/usr/bin/env bash
# Saves the RAM log of the kernel that ran before Freetz started.
# Usage: read-ramlog.sh <log file>. Needs FREETZ_AUTH and RAMLOG_TOOL, see box-test.sh.
set -u
TEST_DIR=$(cd "$(dirname "$0")" && pwd)
. "$TEST_DIR/freetz.sh"
: "${RAMLOG_TOOL:?set RAMLOG_TOOL to fusiv-ramlog built for the box}"
WORK=$(mktemp -d)
remove_work_files() {
    rm -f "$WORK/part.sh" "$WORK/part.txt"
    rmdir "$WORK"
}
trap remove_work_files EXIT

freetz_read_ramlog "$RAMLOG_TOOL" "$1" "$WORK"
