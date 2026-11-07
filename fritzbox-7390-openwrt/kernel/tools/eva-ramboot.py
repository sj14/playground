#!/usr/bin/env python3
"""Load a kernel into RAM through the AVM bootloader EVA.

The sequence follows OpenWrt's scripts/flashing/eva_ramboot.py. The difference
is that this tool waits for EVA and restarts the box if anything goes wrong, so
that it never stays stuck in the bootloader.
"""

import argparse
import os
import socket
import sys
import time
from ftplib import FTP

FTP_USER = "adam2"
RAM_START = 0x80000000
RAM_SIZE = 0x08000000
PAGE_MASK = 0xfff
CONNECT_TIMEOUT_S = 0.4
COMMAND_TIMEOUT_S = 10
DATA_TIMEOUT_S = 120
RETRY_DELAY_S = 0.05


def wait_for_eva(address: str, timeout_s: float) -> FTP:
    deadline = time.monotonic() + timeout_s
    while time.monotonic() < deadline:
        ftp = FTP()
        try:
            ftp.connect(address, 21, timeout=CONNECT_TIMEOUT_S)
        except OSError:
            time.sleep(RETRY_DELAY_S)
            continue
        ftp.sock.settimeout(COMMAND_TIMEOUT_S)
        print(ftp.getwelcome(), flush=True)
        return ftp
    raise SystemExit(f"EVA did not answer within {timeout_s} s")


def adam_command(ftp: FTP, command: str) -> None:
    print(f"> {command}", flush=True)
    reply = ftp.sendcmd(command)
    print(f"< {reply}", flush=True)
    if not reply.startswith("200"):
        raise RuntimeError(f"unexpected reply: {reply}")


def request_reboot(ftp: FTP) -> None:
    """EVA answers REBOOT with two lines, so read neither through ftplib."""
    ftp.sock.sendall(b"REBOOT\r\n")
    print("> REBOOT", flush=True)


def load_image(ftp: FTP, image_path: str) -> None:
    size = os.stat(image_path).st_size
    memsize = (RAM_SIZE - size) & ~PAGE_MASK
    start = RAM_START + memsize
    end = RAM_START + RAM_SIZE

    adam_command(ftp, f"SETENV memsize 0x{memsize:08x}")
    adam_command(ftp, f"SETENV kernel_args_tmp mtdram1=0x{start:08x},0x{end:08x}")
    adam_command(ftp, "MEDIA SDRAM")

    ftp.set_pasv(True)
    ftp.sock.settimeout(DATA_TIMEOUT_S)
    # ftplib would otherwise use the short connect timeout for the data connection too.
    ftp.timeout = DATA_TIMEOUT_S
    upload_start = time.monotonic()
    with open(image_path, "rb") as image:
        print(ftp.storbinary(f"STOR 0x{start:08x} 0x{end:08x}", image), flush=True)
    print(f"uploaded {size} bytes in {time.monotonic() - upload_start:.1f} s", flush=True)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("address")
    parser.add_argument("--wait", type=float, default=240, help="seconds to wait for EVA")
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("reboot", help="leave EVA and boot the installed firmware")
    load_command = commands.add_parser("load", help="load and start a kernel image")
    load_command.add_argument("image")
    args = parser.parse_args()

    ftp = wait_for_eva(args.address, args.wait)
    ftp.login(FTP_USER, FTP_USER)

    if args.command == "reboot":
        request_reboot(ftp)
        return

    try:
        load_image(ftp, args.image)
    except (OSError, EOFError, RuntimeError, socket.error) as error:
        print(f"loading failed: {error!r}", file=sys.stderr, flush=True)
        request_reboot(ftp)
        raise SystemExit(1)

    ftp.close()


if __name__ == "__main__":
    main()
