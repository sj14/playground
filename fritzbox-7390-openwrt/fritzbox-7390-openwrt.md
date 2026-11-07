# OpenWrt on the FRITZ!Box 7390 (hobby project)

As of 2026-11-07. A current kernel boots from RAM on the box and opens a Wi-Fi network. The installed firmware is unchanged.

## Summary

- **Goal:** a current kernel and OpenWrt on my own 7390, as a hobby project.
- **Realistic:** a box with Wi-Fi (`ath9k`) and USB Ethernet as the uplink.
- **Not realistic:** official inclusion in OpenWrt, because the Ethernet driver and its firmware are proprietary.
- **Open decision:** Is "Wi-Fi plus USB Ethernet, LAN ports dead for now" acceptable as the goal?
- **Next step:** a long test under load, the throughput limit of about 24 Mbit/s, and the LAN ports. Linux only runs from RAM so far, installing it would mean writing to flash.

## The box

Values were read from the box unless stated otherwise.

| Property | Value |
|---|---|
| Model | FRITZ!Box Fon WLAN 7390, HWRevision 156, HWSubRevision 3 |
| Firmware | 06.88 with Freetz-NG, web interface on port 81 |
| Kernel | 2.6.28.10 |
| SoC | Ikanos Fusiv (`Ikanos Fusiv Core`), Vx180 at 500 MHz according to the forum |
| CPU | MIPS 24Kc V4.12, big endian, single core |
| RAM | 128 MB (`memsize=128`) |
| NOR | 16 MB, `physmap-flash` at `0x1f000000` |
| NAND | 512 MB, `direct-avm-nand` at `0x1e000000` |
| PCI | memory window `0x1a000000`, two devices `168c:ff1d`, IRQ 25 |
| Wi-Fi | AR9220 and AR9223 according to the forum |
| Switch | AR8316 according to the forum |
| Bootloader | EVA, `bootloaderVersion 1.1939` |
| Console | `console=ttyS0,115200n8r`, UART at `0xb90a0000`, IRQ 29 |

### Flash layout

```text
mtd0  00d85b00  rootfs
mtd1  0015a500  kernel
mtd2  00020000  urlader
mtd3  00080000  tffs (1)
mtd4  00080000  tffs (2)
mtd5  01000000  reserved
mtd6  20000000  nand-filesystem
```

## What exists and what is missing

This is based on AVM's newest GPL package (06.86). The box runs 06.88.

| Part | Status | Evidence |
|---|---|---|
| IRQ, timer, clock, GPIO, setup | open, for 2.6.28 | `arch/mips/fusiv/fusiv_mips32/`, registers in `arch/mips/include/asm/mach-fusiv/vx180.h` |
| Serial console | open | `setup.c`, `fusiv_uart.h` |
| PCI | open | `arch/mips/pci/pci-fusiv.c` |
| NAND and NOR | open | `drivers/mtd/nand/direct_avm.c`, `fusiv_mtd.c` |
| USB | open | `drivers/usb/host/ehci-ikf*.c`, `ohci-ikf*.c`; matching variant not checked yet |
| Wi-Fi | very likely feasible | `ath9k_pci_owl_loader` in the current kernel knows `0xff1d` |
| Ethernet | **closed** | `ethdriver_lkm.ko`: `license=proprietary`, `author=Ikanos Communications` |
| VDSL, telephony, DECT | not planned | probably not needed when used as an access point |

### Ethernet

- `fusiv_src/` contains only headers and Makefiles (the only `.c` file is `fusiv_srcd.c`).
- Packets pass through a network coprocessor. Its firmware is probably `ETHAP_codeSection` (11,236 bytes) in the module, loaded via `loadAPCode` from `fusivlib_lkm`.
- Switch access (`fusiv_mdio_read`, `fusiv_mdio_write`) is also in the closed module.
- Module dependencies: `fusivlib_lkm`, `ap2ap_lkm`, `timers_lkm`, `periap_driver_lkm`.
- For the LAN ports, you would have to load the firmware from your own box and rebuild the host driver through reverse engineering. The headers under `fusiv_src/kernel/ap_code/inc/netpro/` could help. The firmware probably may not be redistributed.

### Wi-Fi

- The radio chips sit directly on the SoC's PCI bus. This speaks against the second Wi-Fi CPU suggested in the forum.
- The ID `ff1d` matches the owl loader, which loads the calibration data from flash. The location is now known, see below.

Calibration data, found read-only in the `urlader` partition (`mtd2`):

| Flash offset | Header | PCI device | Radio |
|---|---|---|---|
| `0x980` | version 2, type `WLAN`, length `0x1001` | `0000:00:01.0` (`wifi0`) | 5 GHz, `opCapFlags=0x01` |
| `0x1985` | version 2, type `WLAN2`, length `0x1001` | `0000:00:02.0` (`wifi1`) | 2.4 GHz, `opCapFlags=0x02` |

- Each header is followed by one padding byte, then 4096 bytes of byte-swapped data starting at `0x985` and `0x198a`. Both sizes fit the owl loader limit, and it handles the swapped form.
- Both blocks pass: magic `0xa55a`, a register list of 12 entries with an end marker, EEPROM version `0xe010`, 2x2 masks, and an XOR checksum of `0xffff` over the declared length `0xcb8`.
- The MAC addresses in the blocks match the `macwlan` and `macwlan2` environment values and the running interfaces.
- The register list sets the PCI ID to `168c:0029`, which `ath9k` supports. `ath9k` reads these calibration blocks through `nvmem`.
- The partition also holds secrets such as `wlan_key` and `tr069_passphrase` in its environment, so a dump of it must not be published.

## Milestones

1. **Kernel to userspace (reached):** Linux 6.18 boots from RAM, without a serial cable. A telnet shell worked (`hardware/first-shell-6.18.log`), but was removed again when both Wi-Fi networks got connected to the home network.
2. **Flash and Wi-Fi (mostly reached):** NOR read-only, PCI and `ath9k` work. A laptop reached the internet over WPA2 on 2.4 GHz and 5 GHz. NAND is still open.
3. **USB Ethernet as uplink (reached):** The USB port needs the board's FPGA for power, and `/init` loads it. Wi-Fi clients reach the internet through a bridge to an AX88179 adapter (`hardware/first-bridged-access-point-6.18.log`).
4. **Optional, LAN ports:** reverse engineering as a separate project.

The kernel work is in `kernel/`, including how to test it without a console.

### Testing without flashing

An image can be loaded into RAM through EVA via FTP, the same way as OpenWrt's `eva_ramboot.py`. This works on the 7390, even over Wi-Fi from a machine in the same network, and the box returns to Freetz after the test.

The box is unavailable as an access point during tests, so a second, used 7390 as a test device would be helpful.

## Serial console

So far there is **no confirmed pinout** and no verified photo for the 7390. The pins have to be measured, because the order differs between models.

### Where to look

- According to a 7390 owner: four solder pads in a row, one of them rectangular.
- On older FRITZ!Boxes, at the board edge with the LEDs. This is not confirmed for the 7390.
- Do not confuse it with the 14-pin connector on the underside, which is JTAG.

### Identifying the pins (with a multimeter)

| Pin | Measurement | Result |
|---|---|---|
| GND | power unplugged, continuity to the metal shell of LAN or USB | beeps |
| 3.3 V | box on, voltage against GND | constant at about 3.3 V, **do not connect** |
| Box TX | voltage against GND during boot | visibly fluctuates |
| Box RX | the remaining pin | only needed later |

If you measure significantly more than 3.3 V anywhere, do not connect anything.

### Connecting a Raspberry Pi

The Pi's GPIOs use 3.3 V, and its 5 V pins must not be used. Start with only two wires, so nothing can be sent to the box. Both devices must be off while connecting, and each keeps its own power supply.

| Box | Pi (physical pin) |
|---|---|
| GND | pin 6 |
| TX | pin 10 (GPIO15, RX) |
| RX, only later | pin 8 (GPIO14, TX) |
| 3.3 V | **do not connect** |

Pins can be soldered in, and the forum also mentions pogo pins as an alternative. A pin header pushed in without soldering can work if the pads are through-holes, but the contact may be unreliable.

### Reading the output

1. In `sudo raspi-config`, under "Interface Options" → "Serial Port", disable the login shell over serial and enable the hardware, then reboot.
2. Start the terminal:

   ```bash
   sudo apt install picocom
   picocom -b 115200 /dev/serial0
   ```

3. Turn on the box. At 115200 baud, the kernel log should appear.

Garbled characters or a silent phase right after power-on may come from the bootloader, since `modetty0` specifies 38400 baud. If the kernel part does not appear either, try the other pin, or `picocom -b 38400` if needed.

### Without a console

In theory, the kernel can write its log to a reserved RAM area, which you read through `/dev/mem` after rebooting into Freetz. This is much slower and untested: the RAM would have to survive the reboot, and early crashes produce no output at all. With the Pi, this is not necessary.

## Open items

- [ ] Decision: goal "Wi-Fi plus USB Ethernet", or first estimate the effort for the LAN ports
- [ ] Multimeter available?
- [ ] Open the box and photograph the row of pads
- [ ] Measure the UART pins and connect the console
- [x] Test the EVA RAM boot on the 7390
- [x] Find where the Wi-Fi calibration data is stored
- [ ] Determine the matching USB driver variant for the Vx180
- [x] Back up clock and GPIO registers (`hardware/registers-hw156.txt`, read with `tools/fusiv-regdump.c`)
- [x] Set up the groundwork for the current kernel (`kernel/`, compiles, untested on hardware)

## Limitations of this check

- The GPL sources were checked for existing files and dependencies and only spot-read. Nothing has been compiled yet.
- The effort estimate of several months for an experienced person is a rough guess.
- The forum information (Vx180, Wi-Fi chips, switch) dates from 2019 and is only partly confirmed by the box.

## Alternative without porting

If it is only about current updates and faster Wi-Fi, a used device with OpenWrt support is far less effort, for example a FRITZ!Box 7530 (from memory; check the OpenWrt device list before buying, including the Wi-Fi status).

## Sources

- [AVM GPL sources for the FRITZ!Box 7390](https://osp.avm.de/fritzbox/fritzbox-7390/)
- [`ath9k_pci_owl_loader.c` in the Linux kernel](https://raw.githubusercontent.com/torvalds/linux/master/drivers/net/wireless/ath/ath9k/ath9k_pci_owl_loader.c)
- [`eva_ramboot.py` patch (7412)](https://lists.infradead.org/pipermail/lede-dev/2018-March/011499.html)
- [OpenWrt forum: OpenWrt on AVM FritzBox 7390](https://forum.openwrt.org/t/openwrt-on-avm-fritzbox-7390/46306)
- [OpenWrt forum: Compatibility with Fritz!Box 7390](https://forum.openwrt.org/t/compatibility-with-fritz-box-7390/112699/2)
- [ip-phone-forum: flashing the 7390 over the serial interface](https://www.ip-phone-forum.de/threads/fritz-box-7390-neu-flashen-%C3%BCber-serielle-schnittstelle-m%C3%B6glich.301164/)
- [ip-phone-forum: UART console on the 7390/7590](https://www.ip-phone-forum.de/threads/telnet-ssh-zugang-auf-fb-7390-7590-%C3%BCber-uart-konsole-einrichten.295979/)
- [ip-phone-forum: adapter for the serial console](https://www.ip-phone-forum.de/threads/welchen-adapter-f%C3%BCr-zugriff-auf-serielle-konsole.303107/)
- [ip-phone-forum: serial console on a 7390](https://www.ip-phone-forum.de/threads/fritz-box-fon-wlan-7390-tot-dead-bricked-repair-possible.252004/latest)
- [ZebraDem wiki: Serial console](https://wiki.zebradem.com/wiki/index.php?title=Serielle_Konsole) (older models, not the 7390)
