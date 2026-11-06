# Linux 6.18 for the FRITZ!Box 7390

Groundwork for milestone 1: a current kernel that boots from RAM to a first userspace program on the serial console.

**Status:** It compiles and the image passes all format checks, but it has never run on the hardware.

## Layout

| Path | Content |
|---|---|
| `linux/` | new files, copied over the kernel tree |
| `linux/arch/mips/fusiv/prom.c` | boot arguments, memory, early console, reset, CP0 timer setup |
| `linux/arch/mips/fusiv/irq.c` | Vx180 interrupt controller |
| `linux/arch/mips/boot/dts/ikanos/` | device tree |
| `linux/arch/mips/configs/fusiv_defconfig` | kernel configuration |
| `patches/` | the few changes to existing kernel files |
| `initramfs/init.c` | first userspace program: prints system info, echoes input, ticks every 5 s |
| `tools/make-eva-image.py` | creates and checks images for the EVA bootloader |

## Build

```bash
cd kernel
nix-shell --run ./build.sh
```

The result is `build/fritzbox-7390.eva`. The first run compiles the MIPS cross compiler, which can take a long time.

## What was tested

- The image tool accepts the kernel partition (`mtd1`) of a real box, including CRC and checksum. That image decompresses to the size in its header.
- The kernel and the image build without warnings. The device tree and `/init` are in the kernel.
- `init` runs under `qemu-mips`, with host `/proc` content.

## What can go wrong first

These are assumptions that only the hardware can confirm:

1. **EVA RAM boot:** `eva_ramboot.py` was written for other models and is untested on the 7390.
2. **LZMA end marker:** Python's encoder writes an end marker into the stream. Whether EVA's decoder accepts that is unknown. Check this first if nothing appears on the console.
3. **Early console speed:** The early console keeps the speed set by EVA, possibly 38400 baud (`modetty0`). The kernel switches to 115200 baud once the serial driver loads.
4. **Timer route:** The interrupt code routes the CP0 timer to CPU line 7 as AVM's kernel does. A hang at `calibrate_delay` or similar points here.
5. **Watchdog:** `prom_init` stops the watchdog with the write AVM's driver uses. Whether EVA starts it at all is unknown.
6. **Memory:** Linux avoids the 8 MB at `0x06000000` that AVM's kernel reserves for the DSL coprocessor.

## Test procedure (not done yet)

Requires the serial console. Nothing is written to flash.

1. Connect the console and read along at 115200 baud (see the main document).
2. Connect a PC directly to a LAN port and give it an address such as `192.168.178.2/24`. EVA listens on `192.168.178.1` according to the environment.
3. Power-cycle the box and run this within the first seconds:

   ```bash
   python3 eva_ramboot.py 192.168.178.1 build/fritzbox-7390.eva
   ```

4. Expected output: the kernel log, then `Hello from userspace`, the system info, and a `tick` line every 5 seconds. Typed characters are echoed.
5. A power cycle returns the box to Freetz.

`eva_ramboot.py` comes from [OpenWrt](https://github.com/openwrt/openwrt/blob/main/scripts/flashing/eva_ramboot.py).
