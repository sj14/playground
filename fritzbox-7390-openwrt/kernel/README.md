# Linux 6.18 for the FRITZ!Box 7390

Groundwork for milestone 1: a current kernel that boots from RAM to a first userspace program on the serial console.

**Status:** The kernel boots from RAM on a real box, without a serial cable, and works as an access point: a laptop on the 2.4 GHz and on the 5 GHz WPA2 network got an address from the home router and reached the internet through a bridge to a USB Ethernet adapter (`../hardware/first-bridged-access-point-6.18.log`). PCI, read-only flash, `ath9k` and USB work. The network ports are still missing, and nothing is installed: the kernel only runs from RAM. The bridge now reaches the home network through the LAN port instead of the USB adapter.

## Firmware

The image needs two files from your own box, which must not be published:

- `BITFILE`: the FPGA bitstream, which powers the USB port.
- `AP_FIRMWARE_DIR`: `fusiv-bmu.bin` and `fusiv-eth.bin`, written by `../tools/extract-ap-firmware.py` from `fusivlib_lkm.ko` (prefix `BMUAP`) and `ethdriver_lkm.ko` (prefix `ETHAP`).

Without the second one, `lan0` can't start and the box has no wired network.

## Layout

| Path | Content |
|---|---|
| `linux/` | new files, copied over the kernel tree |
| `linux/arch/mips/fusiv/prom.c` | boot arguments, memory, early console, reset, CP0 timer setup |
| `linux/arch/mips/fusiv/irq.c` | Vx180 interrupt controller |
| `linux/arch/mips/fusiv/bringup.c` | RAM log, LEDs, watchdog |
| `linux/arch/mips/fusiv/eth.c` | LAN port through the packet processors |
| `linux/arch/mips/boot/dts/ikanos/` | device tree |
| `linux/arch/mips/configs/fusiv_defconfig` | kernel configuration |
| `patches/` | the few changes to existing kernel files |
| `initramfs/init.c` | first userspace program: loads the FPGA, creates the bridge, starts the access points and TCP test services, restarts after 180 s |
| `tools/make-eva-image.py` | creates and checks images for the EVA bootloader |
| `tools/eva-ramboot.py` | loads an image through EVA, restarts the box on failure |

## Build

```bash
cd kernel
nix-shell --run ./build.sh
```

The result is `build/fritzbox-7390.eva`. The first run compiles the MIPS cross compiler, which can take a long time. The password of the test networks is in `build/wifi-password`. The build generates it, because these networks are connected to the home network. Keep it out of the repository.

## What was tested

- The image tool accepts the kernel partition (`mtd1`) of a real box, including CRC and checksum. That image decompresses to the size in its header.
- The kernel and the image build without warnings. The device tree and `/init` are in the kernel.
- On the hardware with a laptop as client, with the test network open and without other clients: 20 of 20 pings at about 2 ms, TCP from the box at 32.8 Mbit/s and to the box at about 55 Mbit/s, at 130 Mbit/s link rate and 65 % signal. The PCI bridge stayed alive. This was one run of 12 s per direction.
- USB Ethernet on the hardware, from a laptop on Wi-Fi through another access point: an ASIX AX88179 behind a VIA hub at 480 Mbit/s, 20 of 20 pings at about 2.8 ms, 48.6 Mbit/s from the box and about 11 Mbit/s to it, one run of 15 s per direction. The laptop's Wi-Fi may have limited both values. Telnet refused connections on this address, as intended.
- Bridged access points on the hardware, with a laptop as the only client on one band at a time: address from the home router by DHCP, 8 of 8 pings each to the router, the box and the internet (about 15 ms), and an HTTPS download at 29.2 Mbit/s on 5 GHz and 27.4 Mbit/s on 2.4 GHz, at 130 Mbit/s link rate. One run of 12 s per band.
- `init` runs under `qemu-mips`, with host `/proc` content, including the restart attempt at the end.
- On the hardware: `tools/eva-ramboot.py` loaded the image over Wi-Fi from a machine in the same network, and `fusiv-ramlog` read the log under Freetz. The log shows the timer (about 250 interrupts per second) and UART interrupts through the interrupt controller. `memsize` was back to `0x08000000` after each test.
- Not tested: Anything on the physical serial line, since no cable was connected. Whether the hardware watchdog resets the box, since no run hung.

## Testing without a console

The kernel contains bring-up aids in `linux/arch/mips/fusiv/bringup.c`:

- **RAM log:** All kernel messages also go to 256 KB of RAM at `0x01c00000`. `../tools/fusiv-ramlog.c` reads them in the AVM firmware. The location comes from `fusiv-ramlog scan`. Two earlier locations did not work, because the firmware overwrote the log there.
- **Reboot status:** The kernel and `/init` write known texts to AVM's reboot status mailbox at `0x00fffe00`, which has survived resets on this box. Freetz then shows the progress in `/proc/sys/urlader/reboot_status`: `software` means `prom_init` was never reached, `poweron` means it was, and `watchdog` means `/init` ran. After unplugging the box, the value says nothing.
- **Memory marks:** Right before a restart the kernel marks the RAM every 64 KB. `fusiv-ramlog scan` shows which marks survive under Freetz.
- **LEDs:** Internet = `prom_init` reached, Festnetz = interrupt setup reached, WLAN = timer setup reached, Info = timer interrupts work. All four on was confirmed on the hardware.
- **Watchdog:** It runs from the start, so a hang should restart the box. Whether it really resets the chip is unconfirmed. If the box hangs, unplug it, which also loses the log.
- **Automatic end:** `/init` restarts the box after `TEST_DURATION_S` seconds, 180 by default. A panic restarts it after 10 s. `TICK_S` and `STATUS_INTERVAL_S` set how often `/init` logs. A long run should raise them, because the RAM log stops when it is full.

Procedure, from a machine with an address in `192.168.178.0/24` that reaches the box:

1. Run `tools/eva-ramboot.py 192.168.178.1 load build/fritzbox-7390.eva`. It waits for EVA.
2. Restart the box from Freetz (`/sbin/reboot`). The connection holds the box in EVA until the image is loaded.
3. Wait until Freetz is back, about 2 minutes.
4. Copy `fusiv-ramlog` to `/var/tmp` on the box and run `fusiv-ramlog dump`.
5. Compare `memsize` in `/proc/sys/urlader/environment` with `0x08000000`.

`tools/eva-ramboot.py 192.168.178.1 reboot` leaves EVA if the box stays there.

## Testing with a console (not done yet)

Requires the serial console. The kernel itself is only loaded into RAM, but `eva_ramboot.py` also runs `SETENV memsize` and `SETENV kernel_args_tmp`. Whether those values persist in the flash environment is unconfirmed, so compare `memsize` with the backup afterwards (normally `0x08000000`).

1. Connect the console and read along at 115200 baud (see the main document).
2. Connect a PC directly to a LAN port and give it an address such as `192.168.178.2/24`. EVA listens on `192.168.178.1` according to the environment.
3. Power-cycle the box and run this within the first seconds:

   ```bash
   python3 eva_ramboot.py 192.168.178.1 build/fritzbox-7390.eva
   ```

4. Expected output: the kernel log, then `Hello from userspace`, the system info, and a `tick` line every 5 seconds. Typed characters are echoed.
5. A power cycle returns the box to Freetz.

`eva_ramboot.py` comes from [OpenWrt](https://github.com/openwrt/openwrt/blob/main/scripts/flashing/eva_ramboot.py).

## Image size limit

EVA stopped responding while receiving a 5.9 MB image and needed a power cycle, twice. A 3.2 MB image works. It may overwrite itself when an image reaches too far down from the top of RAM, but that is unconfirmed. Keep the image small or change the load address before going over this size.

## PCI bridge problems

Both are handled in `patches/0003-ath9k-work-around-fusiv-pci-bridge.patch`. When they happen, the registers of the bridge itself read as zero, the chips stop answering, and the unserved Wi-Fi interrupt ends in `irq 25: nobody cared`.

- **Reading analog registers (`0x7800` to `0x78ff`) kills the bridge.** Reads of `0x7860` under the AVM firmware and `0x7894` under this kernel both did it, but `0x7810` and `0x7834` did not, so the range is a safe generalization. The patch answers these reads from a copy of the last written values. Do not read this range from tools, because it takes down the Wi-Fi of the running firmware too.
- **Too many register writes in flight end in a master abort.** The patch reads from the chip after each write, as far as it is allowed, and pauses after analog writes.

The chips get their real PCI ID in the first bus scan (`arch/mips/pci/pci-fusiv.c`), so `ath9k_pci_owl_loader` is not used. With the owl loader, the second chip read ID 0, which is explained by the first problem.

## Running a test

Test runs are not scripted in this repository, because they need the box's password. A run does this:

1. Wait until the box has been up for 90 s, because `/sbin/reboot` is ignored early and sometimes later too, so repeat it until the box goes down.
2. Run `ifconfig ath0 down` and `ifconfig ath1 down` on the box, so that a machine using the box's Wi-Fi moves to another access point. It can't see EVA through the box.
3. Make sure that the machine has `192.168.178.2/24`. `nmcli dev modify wlp5s0 +ipv4.addresses 192.168.178.2/24` needs no root.
   Occasionally a scan from the machine did not show the test networks during a whole run, although the box log showed `AP-ENABLED`. The cause is unknown, and repeating the run helped.
4. Follow the procedure in "Testing without a console".

## USB power needs the FPGA

The USB port only gets power after the board's Xilinx FPGA (Spartan XC3S250E) is programmed. Without that, EHCI and OHCI start and report port power, but no device is seen and interrupt 35 stays at zero. A keyboard with backlight showed it: it stays dark under Linux without the FPGA, and it lights up about 13 s after the start of the AVM kernel, when the firmware loads the FPGA. With the FPGA loaded by `/init`, Linux finds the keyboard (see the USB Ethernet log in `../hardware/`).

`/init` programs the FPGA in slave serial mode through GPIOs (`PROG` 14, `CLK` 11, `DATA` 12, `DONE` 8). The bitstream belongs to AVM and is not in this repository. Copy `/lib/modules/bitfile.bit` from your own box and pass its path when building:

```bash
BITFILE=/path/to/bitfile.bit nix-shell --run ./build.sh
```

Without `BITFILE` the image builds and runs as before, but USB has no power.

`usbcore.autosuspend=-1` is also needed, because the controller otherwise stops with the root hub suspended.

Under the AVM firmware the keyboard also goes off for about 3 s after coming on. The cause is unconfirmed. What the FPGA does besides USB power is unknown.

The LEDs show boot progress as intended: with Linux running, all four are on, also with the AVM setting that turns them off in normal operation.
