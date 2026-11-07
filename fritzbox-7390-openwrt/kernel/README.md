# Linux 6.18 for the FRITZ!Box 7390

Groundwork for milestone 1: a current kernel that boots from RAM to a first userspace program on the serial console.

**Status:** The kernel boots from RAM on a real box and restarts cleanly, without a serial cable. PCI, read-only flash and `ath9k` work, and `hostapd` reports `AP-ENABLED`. The network is not visible yet, because the PCI bridge stops responding and the Wi-Fi interrupt then gets disabled (`../hardware/first-access-point-6.18.log`). Network and USB are still missing.

## Layout

| Path | Content |
|---|---|
| `linux/` | new files, copied over the kernel tree |
| `linux/arch/mips/fusiv/prom.c` | boot arguments, memory, early console, reset, CP0 timer setup |
| `linux/arch/mips/fusiv/irq.c` | Vx180 interrupt controller |
| `linux/arch/mips/fusiv/bringup.c` | RAM log, LEDs, watchdog, test deadline |
| `linux/arch/mips/boot/dts/ikanos/` | device tree |
| `linux/arch/mips/configs/fusiv_defconfig` | kernel configuration |
| `patches/` | the few changes to existing kernel files |
| `initramfs/init.c` | first userspace program: logs system info, ticks every 5 s, restarts after 30 s |
| `tools/make-eva-image.py` | creates and checks images for the EVA bootloader |
| `tools/eva-ramboot.py` | loads an image through EVA, restarts the box on failure |

## Build

```bash
cd kernel
nix-shell --run ./build.sh
```

The result is `build/fritzbox-7390.eva`. The first run compiles the MIPS cross compiler, which can take a long time.

## What was tested

- The image tool accepts the kernel partition (`mtd1`) of a real box, including CRC and checksum. That image decompresses to the size in its header.
- The kernel and the image build without warnings. The device tree and `/init` are in the kernel.
- `init` runs under `qemu-mips`, with host `/proc` content, including the restart attempt at the end.
- On the hardware: `tools/eva-ramboot.py` loaded the image over Wi-Fi from a machine in the same network, and `fusiv-ramlog` read the log under Freetz. The log shows the timer (about 250 interrupts per second) and UART interrupts through the interrupt controller. `memsize` was back to `0x08000000` after each test.
- Not tested: Anything on the physical serial line, since no cable was connected. Whether the hardware watchdog resets the box, since no run hung.

## Testing without a console

The kernel contains bring-up aids in `linux/arch/mips/fusiv/bringup.c`:

- **RAM log:** All kernel messages also go to 256 KB of RAM at `0x01c00000`. `../tools/fusiv-ramlog.c` reads them in the AVM firmware. The location comes from `fusiv-ramlog scan`. Two earlier locations did not work, because the firmware overwrote the log there.
- **Reboot status:** The kernel and `/init` write known texts to AVM's reboot status mailbox at `0x00fffe00`, which has survived resets on this box. Freetz then shows the progress in `/proc/sys/urlader/reboot_status`: `software` means `prom_init` was never reached, `poweron` means it was, and `watchdog` means `/init` ran. After unplugging the box, the value says nothing.
- **Memory marks:** Right before a restart the kernel marks the RAM every 64 KB. `fusiv-ramlog scan` shows which marks survive under Freetz.
- **LEDs:** Internet = `prom_init` reached, Festnetz = interrupt setup reached, WLAN = timer setup reached, Info = timer interrupts work.
- **Watchdog:** It runs from the start, so a hang should restart the box. Whether it really resets the chip is unconfirmed. If the box hangs, unplug it, which also loses the log.
- **Automatic end:** `/init` restarts the box after 120 s, and the kernel does so after 180 s at the latest.

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
