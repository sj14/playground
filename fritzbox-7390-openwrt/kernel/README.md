# Linux 6.18 for the FRITZ!Box 7390

Groundwork for milestone 1: a current kernel that boots from RAM to a first userspace program on the serial console.

**Status:** It compiles and the image passes all format checks, but it has never run on the hardware.

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
- `tools/eva-ramboot.py` has not loaded anything yet. `fusiv-ramlog` has not run yet.

## What can go wrong first

These are assumptions that only the hardware can confirm:

1. **EVA RAM boot:** `eva_ramboot.py` was written for other models and is untested on the 7390.
2. **LZMA end marker:** Python's encoder writes an end marker into the stream. Whether EVA's decoder accepts that is unknown. Check this first if nothing appears on the console.
3. **Early console speed:** The early console keeps the speed set by EVA, possibly 38400 baud (`modetty0`). The kernel switches to 115200 baud once the serial driver loads.
4. **Timer route:** The interrupt code routes the CP0 timer to CPU line 7 as AVM's kernel does. A hang at `calibrate_delay` or similar points here.
5. **Watchdog:** `prom_init` starts the watchdog with the writes AVM's driver uses. On a running box its counter drops by about 10170 per second, so the 0xfffc reload lasts about 6.4 s. A timer serves it every 100 ms. What happens when it expires is unconfirmed.
6. **Memory:** Linux avoids the 8 MB at `0x06000000` that AVM's kernel reserves for the DSL coprocessor.

## Testing without a console (not done yet)

The kernel contains bring-up aids in `linux/arch/mips/fusiv/bringup.c`:

- **RAM log:** All kernel messages also go to 256 KB of the DSL coprocessor's memory at `0x06240000`. Under the AVM firmware, that part held only 23 non-zero words of 65536, while the end of the area is full of data. `../tools/fusiv-ramlog.c` reads them in the AVM firmware. A software reset keeps the AVM reboot status in RAM, so the log should survive too, but this is unconfirmed for this address.
- **Reboot status:** The kernel also writes known texts to AVM's reboot status mailbox at `0x00fffe00`, which has survived resets on this box. Freetz then shows the progress in `/proc/sys/urlader/reboot_status`: `software` means `prom_init` was never reached, `poweron` means it was, and `nmi` means timer interrupts work. After unplugging the box, the value says nothing.
- **LEDs:** Internet = `prom_init` reached, Festnetz = interrupt setup reached, WLAN = timer setup reached, Info = timer interrupts work.
- **Watchdog:** It runs from the start, so a hang should restart the box. Whether it really resets the chip is unconfirmed. If the box hangs, unplug it, which also loses the log.
- **Automatic end:** `/init` restarts the box after 30 s, and the kernel does so after 90 s at the latest.

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
