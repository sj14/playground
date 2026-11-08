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
IMAGE=$1
LOG=$2
: "${BOX_BSSID_PREFIX:?set BOX_BSSID_PREFIX to the start of the box BSSIDs}"
: "${RAMLOG_TOOL:?set RAMLOG_TOOL to fusiv-ramlog built for the box}"
WIFI_DEVICE=${WIFI_DEVICE:-wlp5s0}
EVA_ADDRESS=192.168.178.1
LOCAL_EVA_ADDRESS=192.168.178.2/24
WORK=$(mktemp -d)
WORK_FILES="uptime.sh radio-off.sh reboot.sh load.log part.sh part.txt"
remove_work_files() {
    for file in $WORK_FILES; do
        rm -f "$WORK/$file"
    done
    rmdir "$WORK"
}
trap remove_work_files EXIT

current_access_point() {
    nmcli -f IN-USE,BSSID dev wifi list --rescan no | awk '$1 == "*" { print $2 }'
}

# Freetz ignores a restart during its first seconds.
echo 'cut -d. -f1 /proc/uptime' > "$WORK/uptime.sh"
until [ "$(freetz_run "$WORK/uptime.sh" | head -1)" -ge 90 ] 2> /dev/null; do
    sleep 5
done

# Take this machine off the box's radios, so that it still sees EVA.
printf 'ifconfig ath0 down\nifconfig ath1 down\n' > "$WORK/radio-off.sh"
freetz_run "$WORK/radio-off.sh" > /dev/null
for attempt in $(seq 1 40); do
    case "$(current_access_point)" in
        ""|"$BOX_BSSID_PREFIX"*) sleep 2 ;;
        *) break ;;
    esac
done
case "$(current_access_point)" in
    ""|"$BOX_BSSID_PREFIX"*) echo "still on the box's own radio, not restarting it"; exit 4 ;;
esac
if ! ip -4 addr show "$WIFI_DEVICE" | grep -q "${LOCAL_EVA_ADDRESS%/*}/"; then
    nmcli dev modify "$WIFI_DEVICE" +ipv4.addresses "$LOCAL_EVA_ADDRESS" > /dev/null
    sleep 2
fi

python3 -I "$TEST_DIR/../tools/eva-ramboot.py" --wait 200 "$EVA_ADDRESS" load "$IMAGE" > "$WORK/load.log" 2>&1 &
loader=$!
sleep 1
# Freetz sometimes ignores /sbin/reboot, so repeat it while the box is up.
printf '(sleep 1; /sbin/reboot) > /dev/null 2>&1 &\n' > "$WORK/reboot.sh"
while kill -0 "$loader" 2> /dev/null; do
    freetz_is_up && freetz_run "$WORK/reboot.sh" > /dev/null
    for second in $(seq 1 40); do
        kill -0 "$loader" 2> /dev/null || break
        sleep 1
    done
done
wait "$loader" || { echo "loading failed:"; tail -3 "$WORK/load.log"; exit 2; }
echo "loaded: $(tail -1 "$WORK/load.log")"

freetz_wait "${FREETZ_WAIT:-600}" || { echo "Freetz did not come back"; exit 3; }
echo "Freetz is back"

freetz_upload "$RAMLOG_TOOL" /var/tmp/fusiv-ramlog
: > "$LOG"
offset=0
while true; do
    printf 'chmod 755 /var/tmp/fusiv-ramlog\n/var/tmp/fusiv-ramlog dump %s\n' "$offset" > "$WORK/part.sh"
    freetz_run "$WORK/part.sh" > "$WORK/part.txt"
    header=$(head -1 "$WORK/part.txt")
    length=$(echo "$header" | sed -nE 's/.*length=([0-9]+).*/\1/p')
    if ! echo "$header" | grep -q "^magic=0x46534c47 "; then
        echo "no RAM log found: $header"
        exit 5
    fi
    sed -n '2,$p' "$WORK/part.txt" >> "$LOG"
    offset=$((offset + 30000))
    [ "$offset" -lt "$length" ] || break
done
echo "$header"
