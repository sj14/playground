# OpenWrt on the FRITZ!Box 7390 (hobby project)

As of 2026-11-08. A current kernel with OpenWrt 25.12 boots from RAM on the box and works as an access point at the speed of the AVM firmware. The installed firmware is unchanged.

## Summary

- **Goal:** a current kernel and OpenWrt on my own 7390, as a hobby project.
- **Realistic:** a box with Wi-Fi (`ath9k`) and USB Ethernet as the uplink.
- **Not realistic:** official inclusion in OpenWrt as it stands, because the Ethernet driver needs proprietary firmware from AVM's modules.
- **Open decision:** Is "Wi-Fi plus USB Ethernet, LAN ports dead for now" acceptable as the goal?
- **Next step:** install OpenWrt to flash, see Installing. The Wi-Fi reaches the speed of the AVM firmware (80 Mbit/s, the limit of the connection), see Throughput, also under OpenWrt. Open on the LAN side: the other ports and the data rate between them. The LAN port works, see the Ethernet section. Linux only runs from RAM so far, installing it would mean writing to flash. Before the first flash, check that the original AVM image still boots from RAM through EVA, so that comparison runs stay possible.

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

### Throughput

The goal is the speed of the AVM firmware. A laptop downloading a file from the internet through the box over 2.4 GHz reached 78.6-79.6 Mbit/s under AVM, close to the roughly 80 Mbit/s of the connection. AVM's CPU was about 90% busy doing so, against about 8% without traffic. Linux reaches 80 Mbit/s too (2.4 GHz, 40 MHz), with the CPU about 60% busy.

The CPU time per megabyte is a better measure than the speed of a single download, because the connection varies from run to run. It is the data from the LAN port divided by the softirq time in `/proc/stat`: 7.4 MB per second of softirq time with the bridge skipped, 10.1 MB after the later changes in the table below, and 16.2 MB with the write-back cache, enough for about 130 Mbit/s.

What helped, in order:

| Change | Effect |
|---|---|
| USB adapter keeps the IP header aligned (patch 0004) | 29 to 31 Mbit/s |
| LAN port instead of the USB adapter, no GRO, no copy of received packets | 34 to 45 Mbit/s |
| Kernel hardening and debug options off (with the write-through cache) | 51 Mbit/s |
| Packets between LAN and Wi-Fi skip the bridge (`net/bridge/br_fast_forward.c`, patches 0005 and 0009) | 56-57 Mbit/s |
| Call the driver directly, flow hash from the IPv4 header | 69-73 Mbit/s |
| ath9k doesn't schedule while its queue is full, fair queuing quantum of one Ethernet frame (patches 0007, 0008) | 76-78 Mbit/s |
| Write-back instead of write-through cache (`cca=3`) | 80 Mbit/s with the CPU 60% busy |
| Kernel hardening back on (stack protector, zeroed stack variables, hardened slab freelists) | about 1% more CPU time, no change in speed |

Tried without a measurable effect: `-Os` (slower, 50-53 Mbit/s), four OpenWrt patches for MIPS and mac80211 (cache loops, `-mtune=24kc`, `-mno-branch-likely`, airtime quantum, together about 1.5%).

**Write-through cache:** EVA leaves the CPU's cache mode for kernel memory (K0 in CP0 `Config`, read as `0x80248480`) at 0, write-through without write-allocate, and Linux keeps the bootloader's setting unless the command line sets `cca=`. So every store went to the slow memory, and the profile showed mostly stores. `cca=3` selects write-back. All code that the hardware reads uses uncached addresses or writes the cache back first, so this is safe. `memcpy` reached about 60 MB/s under Linux and under AVM, so AVM probably runs with write-through too.

**No accelerator for Wi-Fi:** AVM's `avm_pa_fusiv.c` (in AVM's GPL sources) shows what the packet processors do. From LAN to Wi-Fi, the MAC processor looks up the destination address in a flow table and still passes the packet to the CPU, which sends it through the Wi-Fi driver. From Wi-Fi to LAN, AVM turned the hardware off ("no HW acceleration for ingress WLAN"). So the processors only take work off the CPU between LAN and WAN.

### Ethernet

- **LAN port works (2026-11-07):** `kernel/linux/arch/mips/fusiv/eth.c` loads AVM's firmware into two packet processors (APs), and a Wi-Fi client reached the internet through `lan0` at about 34 Mbit/s, about 45 Mbit/s without GRO and without copying received packets, and about 51 Mbit/s with the kernel's stack hardening and debug options off. See `hardware/first-lan-port-6.18.log`.
- **How packets move:** The buffer manager at `0x19210000` asks for free buffers one by one with status bit `0x1000`, 4081 in total, and only then answers its configuration. The MAC processor at `0x19260000` writes received buffers to four queues in RAM, whose indexes are in shared memory at `0x191f0040`. To send, the driver takes a buffer from `0x19210810`, fills the header, sets the first egress port to `0x19260510` and writes the address there.
- **Details that cost time:** AVM forwards received packets to a third processor at `0x19100510`, which doesn't run here, so the configuration sends them to the host. In 3 of 8 runs no packets arrived, and port 0 read back as `0x3e` or `0x7e` independently of that. So MDIO writes look unreliable when they follow each other too quickly. `/init` now waits 0.5 ms after each MDIO command and logs what it reads back. All 7 runs since then received packets.
- **Limit:** 2.4 GHz with 40 MHz (270 Mbit/s link) reached about 51 Mbit/s too, the same as 5 GHz with 20 MHz (130 Mbit/s link), with the CPU fully busy in both. So the CPU is the limit, not the radio. 5 GHz with 40 MHz failed twice with `AP-STA-POSSIBLE-PSK-MISMATCH`, cause unknown.
- **Link speed (2026-11-07):** PHY 3, the port with the cable to the home network, negotiated 1000 Mbit/s full duplex (`/init` logs register `0x11` of each PHY). The data rate between two LAN ports at that speed has not been measured, it needs a second wired device.
- **All four LAN sockets (2026-11-08):** Under OpenWrt from RAM, the cable to the home network was moved from socket to socket while `tools/fusiv-links.c` watched the PHYs. Each socket negotiated 1000 Mbit/s full duplex, and through each one the box stayed reachable over SSH and reached the home router. LAN 1 is PHY 0, LAN 2 is PHY 1, LAN 3 is PHY 2 and LAN 4 is PHY 3. PHY 4 never had a link. The data rate between two sockets is still not measured: a laptop with a USB-C Ethernet adapter showed no network device under Linux, so a second wired device is still missing.
- **Open:** The MAC address of `lan0` is random. Link speeds other than the current one are not handled.
- **Switch reached (2026-11-07):** `/init` releases the switch from reset (GPIO 15, active low) and reads it over MDIO at `0x19110080`. It reports id `0x1001`, and the PHYs report `0x004d 0xd041`. PHY 3 lost its link when the cable to the home network was pulled and got it back when it was plugged in again. It is LAN 4. See `hardware/first-switch-6.18.log` and `hardware/first-link-change-6.18.log`.

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
- **WPA3 (2026-11-08):** Under OpenWrt with `encryption=sae-mixed` (WPA2/WPA3), a laptop connected to both bands once with SAE only and once with PSK only, and reached the home router each time. hostapd logged `auth_alg=sae` and `auth_alg=open`. The image uses `wpad-basic-mbedtls`, and the kernel has the AES-CMAC that the required management frame protection needs. The test image now uses `sae-mixed`.

### LEDs

- **Switchable under OpenWrt (2026-11-08):** The LEDs are GPIOs 22 to 28, active low, as in AVM's hw156 configuration: info green 22, info red 23, power green 24, power red 25, Festnetz 26, WLAN 27, Internet 28. `kernel/linux/arch/mips/fusiv/gpio.c` drives GPIOs 16 to 31 with Linux's generic MMIO GPIO code, and the device tree lists the LEDs for `gpio-leds`. On the box, power and WLAN lit as expected, and switching both off through `uci` (as in LuCI under System, LED Configuration) left all LEDs dark.
- **Defaults:** Power shows OpenWrt's boot state through the `led-boot`, `led-running`, `led-failsafe` and `led-upgrade` aliases, WLAN is on while the 2.4 GHz network is up (`kernel/openwrt/files/etc/board.d/01_leds`), and the others stay off.
- **Not used:** GPIOs 0 to 15 get no driver, because `/init` drives the FPGA and the switch reset there. The buttons (DECT 29, WLAN 31) are not set up yet. `MAC80211_LEDS` is off, because ath9k would drive a GPIO of the radio chip whose wiring is unknown.

## Milestones

1. **Kernel to userspace (reached):** Linux 6.18 boots from RAM, without a serial cable. A telnet shell worked (`hardware/first-shell-6.18.log`), but was removed again when both Wi-Fi networks got connected to the home network.
2. **Flash and Wi-Fi (mostly reached):** NOR read-only, PCI and `ath9k` work. A laptop reached the internet over WPA2 on 2.4 GHz and 5 GHz. NAND is still open.
3. **USB Ethernet as uplink (reached):** The USB port needs the board's FPGA for power, and `/init` loads it. Wi-Fi clients reach the internet through a bridge to an AX88179 adapter (`hardware/first-bridged-access-point-6.18.log`).
4. **LAN port (reached):** `lan0` works through the packet processors, see Ethernet.
5. **OpenWrt (reached from RAM, 2026-11-08):** OpenWrt 25.12 with LuCI from the ImageBuilder for `ath79`, with this kernel, see `kernel/README.md`. From RAM the box came up as an access point at `192.168.0.250` after about 2 minutes, answered over SSH and LuCI, and a laptop on the 2.4 GHz network downloaded at 70-81 Mbit/s. Problems on the way: `wifi-scripts` and `iw` come only with `kmod-ath9k`, OpenWrt 25.12 switches Wi-Fi networks off in `wifi-iface` instead of `wifi-device`, and without `CONFIG_BRIDGE_VLAN_FILTERING` the kernel refuses netifd's bridge settings, so `br-lan` never appeared.

The kernel work is in `kernel/`, including how to test it without a console.

### Testing without flashing

An image can be loaded into RAM through EVA via FTP, the same way as OpenWrt's `eva_ramboot.py`. This works on the 7390, even over Wi-Fi from a machine in the same network, and the box returns to Freetz after the test.

The box is unavailable as an access point during tests, so a second, used 7390 as a test device would be helpful.

### Installing (prepared, not done yet)

EVA boots from the area it calls `mtd1` (`0x9F020000`-`0x9FF00000`, 14.875 MB). The AVM firmware keeps its kernel and its root file system there one after the other; under Freetz they appear as `mtd1` ("kernel", `0x15a500` bytes) and `mtd0` ("rootfs", `0xd85b00` bytes). An EVA image such as `kernel/build/fritzbox-7390.eva` is written there over EVA's FTP with `MEDIA FLSH`, `STOR mtd1` and `REBOOT`, the same way as Freetz's `tools/push_firmware`. The bootloader (`urlader`, `mtd2`) and the settings (`tffs`, `mtd3` and `mtd4`) are never written.

Before the first flash:

1. Back up the area under the current firmware, read only, from `/dev/mtd1ro` and `/dev/mtd0ro` in that order, and compare the MD5 sums with the box. `kernel/test/flash-backup.sh` does this: the Freetz shell cuts its output after 64,000 bytes, so the copy goes in 32 KiB pieces. `/dev/mtdblock1` is not suitable, because its size is not a multiple of 512 bytes and the block device drops the last 256 bytes.
2. Load the backup into RAM through EVA, like a test kernel (`kernel/tools/eva-ramboot.py`). This was done on 2026-11-07: Freetz came up from RAM with `mtdram1=0x87120000,0x88000000`, and `/proc/mtd` showed `kernel_ram` and `rootfs_ram`. So the AVM firmware can still be started for comparison runs after an install, and the backup is known to boot.

Rescue paths after an install:

| Problem | Rescue |
|---|---|
| The kernel hangs or panics | the watchdog restarts the box, EVA answers at every start |
| Back to the AVM firmware | write the backup with `MEDIA FLSH` and `STOR mtd1`, the settings in `tffs` stay |
| Logs of the installed kernel | over the network (`logread`, `dmesg`). The RAM log doesn't help here: the AVM firmware started from RAM found no log of the previous kernel, so it overwrites that area, unlike when it starts from flash |
| EVA itself broken | only JTAG, so `mtd2` is never written |

An installed image needs `TEST_DURATION_S=0`, otherwise `/init` restarts the box after each test period.

Planned layout of an installed OpenWrt, not built yet:

1. The EVA image of the kernel without the OpenWrt files at the start of `mtd1`, in a fixed partition of 4 MB, followed by the OpenWrt root file system as squashfs. The kernel needs the block layer, `mtdblock`, squashfs and overlayfs for that.
2. Linux keeps the flash read only at first (`mtd-rom`), so settings live in RAM and every start begins with the defaults of the image. So the box can't damage its own flash. A writable `rootfs_data` partition with jffs2, and with it saved settings and `sysupgrade`, needs a flash driver that can write (CFI) and comes as a second step.
3. Before the first flash, the same image can be tested from RAM: EVA passes the uploaded area as `mtdram1=start,end`, and if the kernel registers that area as an MTD device with the same partitions, the complete flash image boots from RAM, the same way the AVM firmware uses `kernel_ram` and `rootfs_ram`.

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
