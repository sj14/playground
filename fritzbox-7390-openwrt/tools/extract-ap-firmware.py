#!/usr/bin/env python3
"""Writes the packet processor firmware from AVM's kernel modules to a file.

Usage: extract-ap-firmware.py <nm> <objcopy> <module.ko> <prefix> <output>

The firmware belongs to Ikanos and must not be published, so the output stays
outside the repository, like the FPGA bitstream.

Output format, big endian: eleven 32 bit values, the names in LAYOUT_FIELDS,
followed by the code, data and small data sections.
"""
import struct
import subprocess
import sys
import tempfile

LAYOUT_FIELDS = [
    "codeSectionLength", "dataSectionLength", "sDataSectionLength",
    "startData", "endData", "startSData", "endSData",
    "startBSS", "endBSS", "startSBSS", "endSBSS",
]


def read_symbols(nm, module):
    """Returns {name: (type, value)} of the symbols in the module."""
    symbols = {}
    for line in subprocess.check_output([nm, module], text=True).splitlines():
        parts = line.split()
        if len(parts) == 3:
            symbols[parts[2]] = (parts[1], int(parts[0], 16))
    return symbols


def read_data_section(objcopy, module):
    with tempfile.NamedTemporaryFile() as output:
        subprocess.check_call([objcopy, "-O", "binary", "--only-section=.data", module, output.name])
        return output.read()


def main():
    nm, objcopy, module, prefix, output_path = sys.argv[1:6]
    symbols = read_symbols(nm, module)
    data = read_data_section(objcopy, module)

    def layout_value(field):
        symbol_type, offset = symbols[f"{prefix}_{field}"]
        # Symbols in .bss are zero in the module's image.
        if symbol_type.upper() == "B":
            return 0
        return struct.unpack_from(">I", data, offset)[0]

    def section(name, length):
        offset = symbols[f"{prefix}_{name}"][1]
        return data[offset:offset + length]

    values = [layout_value(field) for field in LAYOUT_FIELDS]
    code = section("codeSection", values[0])
    initialized = section("dataSection", values[1])
    small = section("sDataSection", values[2])

    with open(output_path, "wb") as output:
        output.write(struct.pack(">11I", *values))
        output.write(code + initialized + small)

    for field, value in zip(LAYOUT_FIELDS, values):
        print(f"{prefix}_{field} = {value:#x}")


if __name__ == "__main__":
    main()
