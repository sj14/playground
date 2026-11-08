#!/usr/bin/env bash
# Loads an OpenWrt test image into RAM through EVA, saves its state over SSH and
# restarts it, so that the box returns to Freetz.
# Usage: openwrt-test.sh <image> <SSH private key> <output prefix>
#
# Needs the variables of box-test.sh except RAMLOG_TOOL. The test image comes up
# at 192.168.0.250 (openwrt/uci-defaults-test). Its host key changes with every
# start, so it is not checked.
set -u
TEST_DIR=$(cd "$(dirname "$0")" && pwd)
. "$TEST_DIR/freetz.sh"
. "$TEST_DIR/eva.sh"
IMAGE=$1
KEY=$2
OUT=$3
OPENWRT_ADDRESS=192.168.0.250
WORK=$(mktemp -d)
WORK_FILES="uptime.sh radio-off.sh reboot.sh load.log"
remove_work_files() {
    for file in $WORK_FILES; do
        rm -f "$WORK/$file"
    done
    rmdir "$WORK"
}
trap remove_work_files EXIT

openwrt() {
    ssh -i "$KEY" -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o LogLevel=ERROR \
        -o ConnectTimeout=5 -o BatchMode=yes "root@$OPENWRT_ADDRESS" "$@"
}

eva_load "$IMAGE" "$WORK" || exit $?

start=$(date +%s)
until openwrt true 2> /dev/null; do
    if [ $(( $(date +%s) - start )) -ge 300 ]; then
        echo "no SSH after 300 s, the test image restarts by itself"
        exit 6
    fi
    sleep 5
done
echo "SSH after $(( $(date +%s) - start )) s"
echo "LuCI answers with HTTP $(curl -s -o /dev/null --max-time 10 -w '%{http_code}' "http://$OPENWRT_ADDRESS/cgi-bin/luci")"

# The state report leaves the Wi-Fi keys out.
openwrt test-diagnostics > "$OUT.state" 2>&1
openwrt 'dmesg; logread' > "$OUT.log" 2>&1
openwrt reboot

freetz_wait "${FREETZ_WAIT:-600}" || { echo "Freetz did not come back"; exit 3; }
echo "Freetz is back"
