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
MAKE_ARGS=(ARCH=mips "CROSS_COMPILE=${CROSS_COMPILE:-mips-unknown-linux-gnu-}" -j"$(nproc)")

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

# The first userspace program.
zig cc -target mips-linux-musleabi -mcpu=mips32r2 -static -Os -s \
    -o "$BUILD_DIR/init" "$SOURCE_DIR/initramfs/init.c"
cat > "$TREE/initramfs.list" <<LIST
dir /dev 755 0 0
nod /dev/console 600 0 0 c 5 1
nod /dev/kmsg 600 0 0 c 1 11
nod /dev/mem 600 0 0 c 1 1
dir /proc 755 0 0
file /init $BUILD_DIR/init 755 0 0
LIST

make -C "$TREE" "${MAKE_ARGS[@]}" fusiv_defconfig
make -C "$TREE" "${MAKE_ARGS[@]}" vmlinux.bin

# The kernel must end below the RAM log (FUSIV_RAMLOG_PHYS).
KERNEL_END=$(awk '$3 == "_end" { print $1 }' "$TREE/System.map")
if [ "$((0x$KERNEL_END))" -ge "$((0x80c00000))" ]; then
    echo "The kernel ends at 0x$KERNEL_END, which overlaps the RAM log." >&2
    exit 1
fi
echo "The kernel ends at 0x$KERNEL_END"

ENTRY=0x$(awk '$3 == "kernel_entry" { print $1 }' "$TREE/System.map")
python3 "$SOURCE_DIR/tools/make-eva-image.py" build \
    "$TREE/arch/mips/boot/vmlinux.bin" "$LOAD_ADDRESS" "$ENTRY" "$BUILD_DIR/fritzbox-7390.eva"
python3 "$SOURCE_DIR/tools/make-eva-image.py" verify "$BUILD_DIR/fritzbox-7390.eva"
