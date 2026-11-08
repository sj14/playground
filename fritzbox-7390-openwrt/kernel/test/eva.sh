# Loads an image into RAM through EVA while the box runs Freetz. Source this file
# after freetz.sh. Needs BOX_BSSID_PREFIX, the start of the box's own BSSIDs,
# because this machine must not reach the network through the box's radios, and
# WIFI_DEVICE, this machine's Wi-Fi interface (default wlp5s0).
: "${BOX_BSSID_PREFIX:?set BOX_BSSID_PREFIX to the start of the box BSSIDs}"
WIFI_DEVICE=${WIFI_DEVICE:-wlp5s0}
EVA_ADDRESS=192.168.178.1
LOCAL_EVA_ADDRESS=192.168.178.2/24
EVA_TOOLS=$(cd "$(dirname "${BASH_SOURCE[0]}")/../tools" && pwd)

current_access_point() {
    nmcli -f IN-USE,BSSID dev wifi list --rescan no | awk '$1 == "*" { print $2 }'
}

# Usage: eva_load <image> <work directory>. Returns once EVA started the image.
eva_load() {
    local image=$1
    local work=$2
    local loader attempt second

    if ! command -v python3 > /dev/null; then
        echo "python3 is missing, run the test scripts inside nix-shell"
        return 2
    fi

    # Freetz ignores a restart during its first seconds.
    echo 'cut -d. -f1 /proc/uptime' > "$work/uptime.sh"
    until [ "$(freetz_run "$work/uptime.sh" | head -1)" -ge 90 ] 2> /dev/null; do
        sleep 5
    done

    # Take this machine off the box's radios, so that it still sees EVA.
    printf 'ifconfig ath0 down\nifconfig ath1 down\n' > "$work/radio-off.sh"
    freetz_run "$work/radio-off.sh" > /dev/null
    for attempt in $(seq 1 40); do
        case "$(current_access_point)" in
            ""|"$BOX_BSSID_PREFIX"*) sleep 2 ;;
            *) break ;;
        esac
    done
    case "$(current_access_point)" in
        ""|"$BOX_BSSID_PREFIX"*) echo "still on the box's own radio, not restarting it"; return 4 ;;
    esac
    if ! ip -4 addr show "$WIFI_DEVICE" | grep -q "${LOCAL_EVA_ADDRESS%/*}/"; then
        nmcli dev modify "$WIFI_DEVICE" +ipv4.addresses "$LOCAL_EVA_ADDRESS" > /dev/null
        sleep 2
    fi

    python3 -I "$EVA_TOOLS/eva-ramboot.py" --wait 200 "$EVA_ADDRESS" load "$image" > "$work/load.log" 2>&1 &
    loader=$!
    sleep 1
    # Freetz sometimes ignores /sbin/reboot, so repeat it while the box is up.
    printf '(sleep 1; /sbin/reboot) > /dev/null 2>&1 &\n' > "$work/reboot.sh"
    while kill -0 "$loader" 2> /dev/null; do
        freetz_is_up && freetz_run "$work/reboot.sh" > /dev/null
        for second in $(seq 1 40); do
            kill -0 "$loader" 2> /dev/null || break
            sleep 1
        done
    done
    wait "$loader" || { echo "loading failed:"; tail -3 "$work/load.log"; return 2; }
    echo "loaded: $(tail -1 "$work/load.log")"
}
