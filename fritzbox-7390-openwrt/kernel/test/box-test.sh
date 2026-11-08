#!/usr/bin/env bash
# Loads an image into RAM through EVA, lets it run and saves its RAM log after the
# box is back under Freetz. Usage: box-test.sh <image> <log file>
#
# Besides FREETZ_URL and FREETZ_AUTH (see freetz.sh):
#   BOX_BSSID_PREFIX  start of the box's own BSSIDs, such as "AA:BB:CC:". This
#                     machine must not reach the network through the box's radios.
#   WIFI_DEVICE       this machine's Wi-Fi interface, default wlp5s0
#   RAMLOG_TOOL       ../tools/fusiv-ramlog.c built for the box
#   FREETZ_WAIT       seconds to wait for Freetz after the upload, default 600
set -u
TEST_DIR=$(cd "$(dirname "$0")" && pwd)
. "$TEST_DIR/freetz.sh"
. "$TEST_DIR/eva.sh"
IMAGE=$1
LOG=$2
: "${RAMLOG_TOOL:?set RAMLOG_TOOL to fusiv-ramlog built for the box}"
WORK=$(mktemp -d)
WORK_FILES="uptime.sh radio-off.sh reboot.sh load.log part.sh part.txt"
remove_work_files() {
    for file in $WORK_FILES; do
        rm -f "$WORK/$file"
    done
    rmdir "$WORK"
}
trap remove_work_files EXIT

eva_load "$IMAGE" "$WORK" || exit $?

freetz_wait "${FREETZ_WAIT:-600}" || { echo "Freetz did not come back"; exit 3; }
echo "Freetz is back"

freetz_read_ramlog "$RAMLOG_TOOL" "$LOG" "$WORK"
