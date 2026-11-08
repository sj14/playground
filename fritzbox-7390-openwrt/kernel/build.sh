#!/usr/bin/env bash
# Builds an EVA image of the kernel for the FRITZ!Box 7390.
# Run it inside "nix-shell". BUILD_DIR selects where the kernel is unpacked.
set -euo pipefail

KERNEL_VERSION=6.18.62
KERNEL_SHA256=490a7aedf1b355c565d3a6c05333ccccb5da82064eca46b590448a7099d7d651
LOAD_ADDRESS=0x80010000

SOURCE_DIR=$(cd "$(dirname "$0")" && pwd)
BUILD_DIR=${BUILD_DIR:-$SOURCE_DIR/build}
TARBALL=$BUILD_DIR/linux-$KERNEL_VERSION.tar.xz
TREE=$BUILD_DIR/linux-$KERNEL_VERSION-fusiv
MAKE_ARGS=(ARCH=mips "CROSS_COMPILE=${CROSS_COMPILE:-mips-unknown-linux-musl-}" -j"$(nproc)")

mkdir -p "$BUILD_DIR"

if [ ! -f "$TARBALL" ]; then
    curl -fL -o "$TARBALL" "https://cdn.kernel.org/pub/linux/kernel/v6.x/linux-$KERNEL_VERSION.tar.xz"
fi
echo "$KERNEL_SHA256  $TARBALL" | sha256sum -c -

if [ ! -d "$TREE" ]; then
    mkdir "$TREE"
    tar -xf "$TARBALL" -C "$TREE" --strip-components=1
fi

for patch_file in "$SOURCE_DIR"/patches/*.patch; do
    stamp=$TREE/.applied-$(basename "$patch_file")
    if [ ! -f "$stamp" ]; then
        patch -d "$TREE" -p1 < "$patch_file"
        touch "$stamp"
    fi
done
cp -r "$SOURCE_DIR/linux/." "$TREE/"

# The first userspace program. With OPENWRT_ROOTFS, the root directory of an
# OpenWrt image from the ImageBuilder, it only prepares the hardware and starts
# OpenWrt's init.
INIT_SOURCE=$SOURCE_DIR/initramfs/init.c
DEFAULT_TEST_DURATION_S=180
if [ -n "${OPENWRT_ROOTFS:-}" ]; then
    INIT_SOURCE=$SOURCE_DIR/initramfs/openwrt-init.c
    DEFAULT_TEST_DURATION_S=600
fi
zig cc -target mips-linux-musleabi -mcpu=mips32r2 -static -Os -s -Wall \
    -DTEST_DURATION_S="${TEST_DURATION_S:-$DEFAULT_TEST_DURATION_S}" \
    -DTICK_S="${TICK_S:-15}" \
    -DSTATUS_INTERVAL_S="${STATUS_INTERVAL_S:-120}" \
    -o "$BUILD_DIR/init" "$INIT_SOURCE" "$SOURCE_DIR/initramfs/board.c"
# The test networks are connected to the home network, so their password is random
# and stays out of the repository.
PASSWORD_FILE=$BUILD_DIR/wifi-password
if [ ! -f "$PASSWORD_FILE" ]; then
    head -c 24 /dev/urandom | base64 | tr -d '/+=' > "$PASSWORD_FILE"
    chmod 600 "$PASSWORD_FILE"
fi
for band in 2ghz 5ghz; do
    sed "s|@PASSWORD@|$(cat "$PASSWORD_FILE")|" "$SOURCE_DIR/initramfs/hostapd-$band.conf" > "$BUILD_DIR/hostapd-$band.conf"
    chmod 600 "$BUILD_DIR/hostapd-$band.conf"
done

cat > "$TREE/initramfs.list" <<LIST
dir /dev 755 0 0
nod /dev/console 600 0 0 c 5 1
nod /dev/kmsg 600 0 0 c 1 11
nod /dev/mem 600 0 0 c 1 1
nod /dev/null 666 0 0 c 1 3
nod /dev/urandom 444 0 0 c 1 9
dir /proc 755 0 0
dir /sys 755 0 0
dir /etc 755 0 0
dir /lib 755 0 0
dir /lib/firmware 755 0 0
file /lib/firmware/regulatory.db $REGDB/regulatory.db 644 0 0
file /init $BUILD_DIR/init 755 0 0
LIST

if [ -z "${OPENWRT_ROOTFS:-}" ]; then
    cat >> "$TREE/initramfs.list" <<LIST
file /etc/hostapd-2ghz.conf $BUILD_DIR/hostapd-2ghz.conf 600 0 0
file /etc/hostapd-5ghz.conf $BUILD_DIR/hostapd-5ghz.conf 600 0 0
dir /usr 755 0 0
dir /usr/sbin 755 0 0
file /usr/sbin/hostapd $HOSTAPD 755 0 0
LIST
else
    # The root password of the test image is random as well. SSH_PUBLIC_KEY may
    # name a key that may log in.
    ROOT_PASSWORD_FILE=$BUILD_DIR/root-password
    if [ ! -f "$ROOT_PASSWORD_FILE" ]; then
        head -c 18 /dev/urandom | base64 | tr -d '/+=' > "$ROOT_PASSWORD_FILE"
        chmod 600 "$ROOT_PASSWORD_FILE"
    fi
    sed -e "s|@WIFI_PASSWORD@|$(cat "$PASSWORD_FILE")|" -e "s|@ROOT_PASSWORD@|$(cat "$ROOT_PASSWORD_FILE")|" \
        "$SOURCE_DIR/openwrt/uci-defaults-test" > "$BUILD_DIR/uci-defaults-test"
    chmod 600 "$BUILD_DIR/uci-defaults-test"
    cat >> "$TREE/initramfs.list" <<LIST
file /etc/uci-defaults/99-fritzbox7390-test $BUILD_DIR/uci-defaults-test 600 0 0
file /etc/board.d/02_network $SOURCE_DIR/openwrt/files/etc/board.d/02_network 755 0 0
file /usr/sbin/test-diagnostics $SOURCE_DIR/openwrt/test-diagnostics 755 0 0
LIST
    if [ -n "${SSH_PUBLIC_KEY:-}" ]; then
        echo "file /etc/dropbear/authorized_keys $SSH_PUBLIC_KEY 600 0 0" >> "$TREE/initramfs.list"
    fi
fi

# The FPGA bitstream belongs to AVM and stays out of the repository. Pass its path
# in BITFILE to include it, for example a copy taken from your own box.
if [ -n "${BITFILE:-}" ]; then
    echo "file /lib/firmware/fpga.bit $BITFILE 600 0 0" >> "$TREE/initramfs.list"
fi

# The firmware of the packet processors belongs to Ikanos and stays out of the
# repository too. tools/extract-ap-firmware.py writes it from AVM's modules.
if [ -n "${AP_FIRMWARE_DIR:-}" ]; then
    echo "file /lib/firmware/fusiv-bmu.bin $AP_FIRMWARE_DIR/fusiv-bmu.bin 600 0 0" >> "$TREE/initramfs.list"
    echo "file /lib/firmware/fusiv-eth.bin $AP_FIRMWARE_DIR/fusiv-eth.bin 600 0 0" >> "$TREE/initramfs.list"
fi

make -C "$TREE" "${MAKE_ARGS[@]}" fusiv_defconfig
if [ -n "${OPENWRT_ROOTFS:-}" ]; then
    # The OpenWrt files belong to the building user, root owns them on the box.
    # The list comes last, so that its files replace those of the image.
    "$TREE/scripts/config" --file "$TREE/.config" \
        --set-str INITRAMFS_SOURCE "$OPENWRT_ROOTFS $TREE/initramfs.list" \
        --set-val INITRAMFS_ROOT_UID "$(id -u)" --set-val INITRAMFS_ROOT_GID "$(id -g)" \
        --enable RD_XZ --disable INITRAMFS_COMPRESSION_NONE --enable INITRAMFS_COMPRESSION_XZ
    make -C "$TREE" "${MAKE_ARGS[@]}" olddefconfig
fi
make -C "$TREE" "${MAKE_ARGS[@]}" vmlinux.bin

# The kernel must end below the RAM log (FUSIV_RAMLOG_PHYS).
KERNEL_END=$(awk '$3 == "_end" { print $1 }' "$TREE/System.map")
if [ "$((0x$KERNEL_END))" -ge "$((0x81c00000))" ]; then
    echo "The kernel ends at 0x$KERNEL_END, which overlaps the RAM log." >&2
    exit 1
fi
echo "The kernel ends at 0x$KERNEL_END"

ENTRY=0x$(awk '$3 == "kernel_entry" { print $1 }' "$TREE/System.map")
python3 "$SOURCE_DIR/tools/make-eva-image.py" build \
    "$TREE/arch/mips/boot/vmlinux.bin" "$LOAD_ADDRESS" "$ENTRY" "$BUILD_DIR/fritzbox-7390.eva"
python3 "$SOURCE_DIR/tools/make-eva-image.py" verify "$BUILD_DIR/fritzbox-7390.eva"
