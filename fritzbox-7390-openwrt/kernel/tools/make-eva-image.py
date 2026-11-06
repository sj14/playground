#!/usr/bin/env python3
"""Create or check a kernel image in the format the AVM bootloader EVA loads.

The format follows lzma2eva from OpenWrt's firmware-utils. "verify" accepts the
kernel partition (mtd1) of a FRITZ!Box 7390, which is how the code was tested.

Layout, all fields little endian even though the target is big endian:

    offset  size  field
    0       4     magic
    4       4     record length (stream size + 24)
    8       4     load address
    12      4     type
    16      4     stream size
    20      4     uncompressed size
    24      4     CRC-32 of the stream
    28      1     LZMA properties
    29      4     LZMA dictionary size
    33      3     zero
    36      n     LZMA stream
    36+n    4     checksum
    40+n    4     zero
    44+n    4     entry point
"""

import argparse
import lzma
import struct
import zlib

MAGIC = 0xfeed1281
TYPE_LZMA = 0x075a0201

HEADER = struct.Struct("<7I")
LZMA_HEADER = struct.Struct("<BI3x")
TRAILER = struct.Struct("<3I")
STREAM_OFFSET = HEADER.size + LZMA_HEADER.size

LC = 3
LP = 0
PB = 2
DICT_SIZE = 8 << 20
LZMA_PROPERTIES = (PB * 5 + LP) * 9 + LC


def lzma_filters(properties: int, dict_size: int) -> list:
    return [{
        "id": lzma.FILTER_LZMA1,
        "lc": properties % 9,
        "lp": (properties // 9) % 5,
        "pb": properties // 45,
        "dict_size": dict_size,
    }]


def checksum_of(image: bytes, stream_size: int) -> int:
    """Length and load address count as words, everything after them as bytes."""
    length, load_address = struct.unpack_from("<2I", image, 4)
    total = length + load_address + sum(image[12:STREAM_OFFSET + stream_size])
    return -total & 0xffffffff


def build(kernel_path: str, load_address: int, entry: int, output_path: str) -> None:
    with open(kernel_path, "rb") as kernel:
        data = kernel.read()

    filters = lzma_filters(LZMA_PROPERTIES, DICT_SIZE)
    stream = lzma.compress(data, format=lzma.FORMAT_RAW, filters=filters)

    image = HEADER.pack(
        MAGIC, len(stream) + 24, load_address, TYPE_LZMA,
        len(stream), len(data), zlib.crc32(stream),
    )
    image += LZMA_HEADER.pack(LZMA_PROPERTIES, DICT_SIZE) + stream
    image += TRAILER.pack(checksum_of(image, len(stream)), 0, entry)

    with open(output_path, "wb") as output:
        output.write(image)

    print(f"{output_path}: kernel {len(data)} bytes, image {len(image)} bytes, entry {entry:#x}")


def verify(image_path: str) -> None:
    with open(image_path, "rb") as image_file:
        image = image_file.read()

    magic, length, load_address, kind, stream_size, size, crc = HEADER.unpack_from(image)
    properties, dict_size = LZMA_HEADER.unpack_from(image, HEADER.size)
    stream = image[STREAM_OFFSET:STREAM_OFFSET + stream_size]
    checksum, zero, entry = TRAILER.unpack_from(image, STREAM_OFFSET + stream_size)

    print(f"load={load_address:#x} entry={entry:#x} stream={stream_size:#x} size={size:#x}")
    print(f"lc/lp/pb properties={properties:#x} dict={dict_size:#x}")

    decoder = lzma.LZMADecompressor(format=lzma.FORMAT_RAW, filters=lzma_filters(properties, dict_size))
    data = decoder.decompress(stream, max_length=size)

    results = {
        "magic": magic == MAGIC,
        "type": kind == TYPE_LZMA,
        "length": length == stream_size + 24,
        "crc": zlib.crc32(stream) == crc,
        "checksum": checksum == checksum_of(image, stream_size),
        "entry record": zero == 0,
        "entry in kernel": load_address <= entry < load_address + size,
        "uncompressed size": len(data) == size,
    }

    for name, passed in results.items():
        print(f"{name}: {'ok' if passed else 'FAILED'}")
    if not all(results.values()):
        raise SystemExit(1)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    commands = parser.add_subparsers(dest="command", required=True)

    build_command = commands.add_parser("build")
    build_command.add_argument("kernel")
    build_command.add_argument("load_address", type=lambda value: int(value, 0))
    build_command.add_argument("entry", type=lambda value: int(value, 0))
    build_command.add_argument("output")

    verify_command = commands.add_parser("verify")
    verify_command.add_argument("image")

    args = parser.parse_args()
    if args.command == "build":
        build(args.kernel, args.load_address, args.entry, args.output)
    else:
        verify(args.image)


if __name__ == "__main__":
    main()
