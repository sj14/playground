# Plan: Slow resume from standby (MacBookPro16,1, T2) – kernel patch for Thunderbolt bridges

- Date: 2026-10-25

## Findings (summary)

- The noirq resume takes ~15–21 s. Almost all of that time goes to the two Thunderbolt upstream bridges
  `0000:06:00.0` (`\_SB.PCI0.PEG1.UPSB`) and `0000:7c:00.0` (`\_SB.PCI0.PEG2.UPSB`), Intel `0x15ea` (Titan Ridge).
- Cause: On resume, Linux calls `acpi_device_set_power(D0)`, which runs `UPSB._PS0`. `_PS0` ends with two calls,
  `CRMW(0x0150, …)` and `CRMW(0x0250, …)`. Each `CRMW` makes up to 5 attempts via `CIOR`
  (polling the CIO mailbox in config space `0x530`, 10,000 × `Stall(25)`, plus `Sleep(100)` per attempt).
- ACPI trace: **10 out of 10 `CIOR` calls per bridge time out, and `CIOW` is never called at all.** `_PS0` ignores the
  return value of `CRMW`, so the waiting achieves nothing.
- `pm_async` barely helps: Only CPU0 is online during resume, and the `Stall` loops busy-wait and block the ACPI
  interpreter. The two bridges therefore run practically one after the other.
- On Apple hardware Linux already identifies as `_OSI("Darwin")` and so takes the firmware's macOS path.
  macOS uses the same ACPI tables. **Decision: do not patch the ACPI table (SSDT12).**
- Already done: KDE switched from hybrid sleep to standby. This removes the slow CPU bring-up (16–36 s)
  and the writing of the hibernation image (~11 s).
- Origin of `pm_async=off`: nixos-hardware `apple/t2/default.nix` (for the old `apple-bce` driver). According to the patch
  comments, the `t2bce` stack in use is compatible with `pm_async`.

## Basic idea

If Linux does not put the Thunderbolt bridges into D3 during system suspend at all (so no `_PS3` runs either), the
ACPI state stays D0. On resume, `acpi_device_set_power()` then skips `_PS0` ("already in D0"), and with it the
`CRMW` timeouts. The firmware table stays untouched.

## Steps

### Step 1: Test without code whether the idea works (~15 min)

- First confirm in the kernel source that `pcie_port_pm=off` leaves bridges in D0 during system suspend
  (`pci_power_manageable()`, `bridge_d3`, `pci_pm_suspend_noirq()`).
- Boot once with the kernel parameter `pcie_port_pm=off`, easiest via the boot menu, without changing the config.
- Test standby with `pm_print_times` and `pm_debug_messages` enabled, optionally with ACPI tracing (`trace_state=enable`). Check:
  - Do `UPSB._PS0` or `CRMW` still run?
  - How long does the noirq resume take?
  - Do USB-C, charging and Thunderbolt hotplug work?
- **Continue** if resume is significantly faster and all devices are present.
- **Stop** if the PCIe link is missing after wakeup (then Linux does need the link training in `PCED`). Continue with step 4.

#### Step 1b: Runtime variant without reboot (current test, 2026-10-25)

`pcie_port_pm=off` can only be set at boot (no entry under `/sys/module/*/parameters`). Instead, the `pcieport` driver is
unbound from the two Thunderbolt bridges only. Assumption (not yet checked in the source): A PCI device without a driver
only has its state saved during suspend and is not put into D3. Then no `_PS3` runs, the ACPI state stays D0,
and `_PS0` is skipped on resume. The bridges have no port services (no `pcie0xx` child nodes); the devices
behind them (xHCI, NHI, downstream ports) keep their drivers.

```
echo 0000:06:00.0 | sudo tee /sys/bus/pci/drivers/pcieport/unbind
echo 0000:7c:00.0 | sudo tee /sys/bus/pci/drivers/pcieport/unbind
# undo: the same with .../bind, or reboot
```

Test conditions: ACPI trace `trace_state=enable`, `pm_print_times=1`, `pm_debug_messages=1`, `pm_async=1`,
`thunderbolt` module loaded.

Check:
- Do `\_SB.PCI0.PEG1.UPSB._PS0` or `PEG2.UPSB._PS0` still run (ACPI trace in `dmesg`)?
- Duration of the noirq resume (reference value with Thunderbolt loaded and `pm_async` on: 17.9 s)?
- USB-C, charging and Thunderbolt hotplug after wakeup?

Result (test on 2026-10-25, with ACPI tracing, which slows everything down a bit):
- **`UPSB._PS0` is no longer called** (both bridges). The assumption holds.
- **noirq resume: 17.9 s → 5.9 s.** Kernel resume from S3 wakeup to "Restarting tasks": ~6.4 s.
- What remains are the two Thunderbolt NHIs `08:00.0` and `7e:00.0` with ~5.8 s each (in parallel). Same pattern:
  `NHI0._PS0` → `PCED` (fast) → `PEG1.CTBT` → `UPSB.CRMW(0x3C, …)` → 5 × `CIOR` timeout, no `CIOW`.
- After that: `00:1b.0` and `00:1c.0` (Wi-Fi root port) with ~1.6 s each.
- xHCI behind the bridges (`09:00.0`, `7f:00.0`): as before, "xHC error in resume … Reinit", which is the known behavior.
- USB-C, charging and hotplug: _to be checked by the user_

Next test: additionally let the NHI wake up without `CTBT`/`CRMW`. Variant A: unload the `thunderbolt` module
(earlier test: then no NHI resume cost). Variant B: unbind only the NHI from its driver
(`/sys/bus/pci/drivers/thunderbolt/unbind`). Expectation: noirq resume ~2 s.

#### Step 1c: Bridges unbound + `thunderbolt` unloaded (test on 2026-10-25, with ACPI tracing)

- No more `UPSB.CRMW`/`CIOR` calls.
- **noirq resume: 1.01 s. Kernel resume from S3 wakeup to "Restarting tasks": 1.54 s** (baseline: 22.4 s).
- Largest remainder: `00:1c.0` (`RP01`, Wi-Fi root port) with 1.0 s.
- After "Restarting tasks", `efivarfs` needs another 1.13 s to re-read the EFI variables before "suspend exit" appears.
  Processes are already running at that point, but `systemd-sleep`, and therefore KDE, only learn about the wakeup afterwards.
- Downside of variant A: Without the `thunderbolt` module, real Thunderbolt devices (docks, TB displays) do not work.
  USB-C with USB and charging go through xHCI and should not be affected.
- USB-C, charging and hotplug: _to be checked by the user_

User check after step 1c: **Charging works, a phone connected via USB-C is detected.**

#### Implementation as a sleep hook (2026-10-25)

- New file `macbook-t2/sleep.nix`, imported in `macbook-t2/configuration.nix` (in my NixOS config,
  `sleep.nix` is staged in git but not yet committed).
- Before sleep (`powerManagement.powerDownCommands`, runs via `sleep-actions.service`): `modprobe -r thunderbolt`,
  then unbind the bridges with the ACPI paths `\_SB_.PCI0.PEG1.UPSB` and `\_SB_.PCI0.PEG2.UPSB` from their driver.
  The bridges are found by ACPI path rather than device ID because the downstream ports have the same ID `0x15ea`.
- After wakeup (`powerManagement.resumeCommands`): bind the bridges to `pcieport` again, `modprobe thunderbolt`.
- Additionally a `systemd.tmpfiles` rule: `pm_async=1`.
- Generated scripts checked with `nix eval`. Read-only dry run: the pattern matches exactly `06:00.0` and `7c:00.0`.
- Untested: rebinding the driver and reloading the module after wakeup (hotplug, `dmesg`).
- ~~Next step: `nixos-rebuild switch`, then test standby and check the logs.~~ Done, see below.

#### Result with the sleep hook (test on 2026-10-25, without tracing)

- **noirq resume 1.01 s, kernel resume from S3 wakeup to "Restarting tasks" 1.51 s** (baseline: 22.4 s, 40–60 s with hybrid sleep).
- `sleep-actions.service` ran before and after sleep without errors (start 0.11 s, stop 0.24 s).
- After wakeup: bridges `06:00.0`/`7c:00.0` bound to `pcieport` again, NHIs `08:00.0`/`7e:00.0` bound to `thunderbolt`,
  Thunderbolt domains `domain0`/`domain1` present, downstream ports `07:xx`/`7d:xx` still bound to `pcieport`.
- `thunderbolt: device links to tunneled native ports are missing!` appears every time the module is loaded, including when it
  was loaded manually during the tests. It is not caused by the rebinding.

Open, minor items (optional):
- `00:1c.0` (`RP01`, Wi-Fi root port): 1.0 s in the noirq resume.
- `efivarfs`: 1.1 s of re-reading the EFI variables after "Restarting tasks", delays the signal to `systemd-sleep` and KDE.
- Commit `sleep.nix`.

## Plan: Upstream patch for t2linux/linux-t2-patches

1. **Open an issue first** at t2linux: findings, ACPI trace (`CRMW`/`CIOR` timeouts, no `CIOW`), measurements and the
   sleep-hook workaround. Ask other users for measurements (`pm_print_times`), especially from other T2 models
   (MBP15,x with Alpine Ridge, MBA, Mac mini, iMac Pro). This clarifies which devices and IDs are affected.
2. **Settle the mechanism in the kernel source** (still to be verified):
   - A: A PCI quirk sets `PCI_DEV_FLAGS_NO_D3` for bridges/NHI. Simplest, but also prevents runtime D3, so it may cost idle power.
   - B (preferred): Only during system suspend: in `suspend_noirq` of `pcieport` or `thunderbolt`, call `pci_save_state()`
     for the affected devices yourself, so the PCI core skips `pci_prepare_to_sleep()`. This matches the tested
     "no driver" behavior.
3. **Matching:** DMI vendor "Apple Inc." plus device IDs (Titan Ridge bridge `0x15ea`, NHI `0x15eb`; upstream port only,
   not the downstream ports with the same ID). Possibly also check whether the ACPI node has a `CRMW` method.
4. **Develop locally:** patch in `macbook-t2/t2-patches/` (directory exists, empty) and include it via `kernelPatches`
   (`kernel-t2.nix` already accepts `args.kernelPatches`). For testing, temporarily remove `sleep.nix` from the imports.
5. **Test:** resume time, USB-C/charging/phone, Thunderbolt device (if available), several cycles, idle power, hibernation.
6. **Submit:** `git format-patch` with a commit message including the ACPI analysis and `Signed-off-by`, as a PR to
   `github.com/t2linux/linux-t2-patches`. Mainline (linux-pci) later, if the t2linux maintainers support it.

### Code review of Linux 7.3 (2026-10-25)

Source: `linux-7.3.tar.xz` from the Nix store.

Confirmed:
- `drivers/pci/pci-driver.c` `pci_pm_suspend_noirq()`: Without a driver (`!pm`), only `pci_save_state()` is called and the code
  then jumps to `set_unknown`. No `pci_prepare_to_sleep()`, so no D3 and no `_PS3`. This is the behavior seen in the tests.
- With a driver: if `state_saved` is already set, `pci_prepare_to_sleep()` is skipped (line ~940). If the device stays
  in D0, the core sets `skip_bus_pm` on the device **and on the bridge above it** (line ~955). A child in D0 therefore keeps the
  bridges above it in D0.
- `drivers/acpi/device_pm.c` `acpi_device_set_power(D0)`: If the cached ACPI state is D0 and there is no `_PSC`, the code
  jumps to `no_change`, so `_PS0` does not run. **SSDT12 contains no `_PSC` at all.**
- `pci_fixup_suspend` quirks run in `pci_pm_suspend_late()`, after `state_saved` has been reset in `pci_pm_suspend()`
  and before `suspend_noirq`. `pci_save_state()` can be called there.
- The NHI driver (`nhi_pm_ops`) does not set `DPM_FLAG_SMART_SUSPEND` and is therefore brought into the active state before
  suspend. `pcieport` sets `SMART_SUSPEND` (a runtime-suspended bridge would stay in D3; in our case the bridges were `active`).
- Precedent in `drivers/pci/quirks.c`: `quirk_apple_poweroff_thunderbolt` (Cactus Ridge). It checks `x86_apple_machine`,
  `pm_suspend_via_firmware()`, and whether ACPI methods exist on the bridge node (which excludes external devices with the same ID).
- The IDs already exist: `PCI_DEVICE_ID_INTEL_TITAN_RIDGE_4C_BRIDGE` (0x15ea) and `..._4C_NHI` (0x15eb) in `drivers/thunderbolt/nhi.h`.

Draft (variant B as a PCI quirk):
- `DECLARE_PCI_FIXUP_SUSPEND` for the Titan Ridge NHI (0x15eb) and for the upstream bridge (0x15ea, only
  `PCI_EXP_TYPE_UPSTREAM`): if `x86_apple_machine && pm_suspend_via_firmware()` and the ACPI method `CRMW` exists
  (on the UPSB node), call `pci_save_state(dev)`. The device stays in D0, ACPI stays at D0, and `_PS0` is skipped on resume.
- The bridge alone is not enough (children are not kept in D0 by their parents, but parents are kept in D0 by their children),
  hence both IDs. With the NHI in D0, `07:00.0` and `06:00.0` stay in D0 anyway. The bridge quirk covers the case where
  `thunderbolt` is not loaded.

Differences from the tested state, to be checked during testing:
- The NHI stays in D0 **with** its driver bound. Does `nhi_resume_noirq()` / `tb_domain_resume_noirq()` recover cleanly even
  though the controller still loses power in S3?
- Because of `skip_bus_pm`, the root ports `00:01.1`/`00:01.2` (`PEG1`/`PEG2`) also stay in D0, so their `_PS0`
  (`PCEU`, fast) is skipped as well. Check the link and the xHCI behind the bridges.
- Power consumption in S3 (overnight) and at idle.

### Patch written and included (2026-10-25)

- File: `macbook-t2/t2-patches/9006-PCI-Keep-Apple-Titan-Ridge-Thunderbolt-in-D0-for-suspend.patch` (staged).
  9006 is the next free number in the 9xxx range. The patch was first named 9001, but t2linux already has that number
  (`9001-ACPI-x86-Apple-T2-systems-need-early-CPU-offlining.patch`), then temporarily 9901. Incidentally, the t2linux 9001 patch
  explains why the CPUs are taken offline early during normal suspend.
- Content: `quirk_apple_titan_ridge_keep_d0()` in `drivers/pci/quirks.c` as `DECLARE_PCI_FIXUP_SUSPEND` for 0x15ea and 0x15eb.
  Conditions: `x86_apple_machine`, `pm_suspend_via_firmware()`, PCIe upstream bridge (two levels up for the NHI) with the
  ACPI method `CRMW`. Then `pci_save_state()`. Debug message: "quirk: keeping Thunderbolt controller in D0 for suspend"
  (`pci_dbg`, visible via dynamic debug).
- `kernel-t2.nix`: additionally reads `./t2-patches/*.patch`, applied after the t2linux patches.
- `sleep.nix`: hook disabled via `useSleepHook = false`, the `pm_async` rule stays.
- Checked: `patch --dry-run` against the unmodified `quirks.c` from 7.3 succeeds (no t2linux patch modifies `quirks.c`). `nix eval` shows the
  patch as the last entry in `kernelPatches`; `sleep-actions` no longer contains `modprobe`.
- **Not checked: compilation.** That only happens during `nixos-rebuild`.

Test after the rebuild:
- Optionally enable the debug message: `echo 'file quirks.c +p' > /sys/kernel/debug/dynamic_debug/control`.
- Enable `pm_print_times` and `pm_debug_messages`, ACPI trace `trace_state=enable` (do `UPSB._PS0`/`NHI0._PS0` still run?).
- Standby: resume time, Thunderbolt driver and domains after wakeup, USB-C/charging/phone, `dmesg` for errors.

## Side finding: Wi-Fi root port `00:1c.0` (`RP01`), ~1 s each on suspend and resume

Read-only (ACPI tables, config space read-only). No tests while the Touch ID session is running over the T2 network.

- Resume: `RP01._PS0` → `ALPR(0)` → `APPU` → `LNKW(0)`. `LNKW(0)` waits up to 1 s (`Timer + 0x989680`) until
  `LACT == 1` (link active) **and** `ARPT.AVND != 0xFFFF` (vendor ID of the Wi-Fi chip `05:00.0` readable). Measured 1.01 s,
  so the wait practically always runs to completion.
- Suspected cause (not proven): Linux calls `_PS0` in `pci_power_up()` **before** `pci_restore_state()`. After S3 the
  root port's secondary bus number is still 0, so a config read of `ARPT` (bus 5) only returns 0xFFFF. `APPU` restores the
  bus numbers only if `SECB == 0xFF` (`BNIR = BNIS`), not if it is 0.
- Additionally: `_TTS` sets `SLTP` to the sleep state (3). During the noirq phase of resume, `SLTP` is still 3, so
  `APPU` skips releasing the reset and the clock (`CKGP`/`CLKD`/`DRST(0)`/`L23R(0)`).
- Suspend: `APPD` → `LNKW(1)` waits up to 1 s for `LACT == 0`. Measured `pci_pm_suspend_noirq` 1.02 s, so here too the wait
  probably runs to completion.
- Link capability checked: the root port reports Data Link Layer Link Active Reporting (LnkCap bit 20 = 1), so `LACT` works.
- Proof (later, without changing the device): ACPI trace in opcode mode for `\_SB.PCI0.RP01.APPU` only; this shows the
  values read for `SECB`, `LACT` and `AVND`.
- Possible fix: as with the Thunderbolt quirk, keep `RP01` in D0 during suspend (saves ~2 s). Open question: whether `APPD`
  (reset of the Wi-Fi chip, L2/L3 handshake) is needed for power consumption in S3 or for brcmfmac.

## nixos-hardware `apple-t2`: is it still needed?

Yes, but only for part of it:
- Overridden or unused: kernel (`boot.kernelPackages` with `mkForce`), `apple-bce` in the initrd (disabled),
  option `kernelChannel` (no effect).
- Still needed:
  - Audio profiles from t2-better-audio: udev rules and patched pipewire/wireplumber/pulseaudio. Verified as active:
    card `AppleT2x6` has `PULSE_PROFILE_SET=apple-t2x6.conf`.
  - Wi-Fi/Bluetooth firmware (`firmware.enable`).
  - `apple-gmux force_igd=y` (`enableIGPU`).
  - `powerManagement.enable`.
  - `intel_iommu=on iommu=pt`.
- Unwanted: `pm_async=off`, overridden via tmpfiles in `sleep.nix`.
- Option: Move the needed parts into a separate file and remove the module. That makes you independent of its
  kernel/parameter changes, but you have to maintain the audio files yourself.

Conclusion for step 2: The quirk should keep the Thunderbolt upstream bridges (`0x15ea`) **and** the NHIs (`0x15eb`) in D0
during system suspend so that their `_PS0` does not run. Then the `thunderbolt` module does not need to be unloaded.
To be clarified: whether the `thunderbolt` driver itself correctly reinitializes an NHI left in D0 after S3.

### Step 2: Targeted quirk instead of a global parameter (~1–2 h)

- PCI quirk in `drivers/pci/quirks.c`: only on Apple hardware (DMI vendor "Apple Inc.") and only for Intel `0x15ea`
  (Thunderbolt upstream bridge, Titan Ridge). These bridges stay in D0 during system suspend.
- Precedents: existing Apple Thunderbolt quirks (`quirk_apple_poweroff_thunderbolt` and similar), which as far as known only cover older controllers.
- Include it as an additional patch in `macbook-t2/kernel-t2.nix` (in my NixOS config).

### Step 3: Measure side effects (~30 min plus a few days of everyday use)

- Compare idle power consumption with and without the patch (`upower`/`powertop`).
- Several standby cycles, USB-C hotplug, charging.

### Step 4: Only if step 1 fails – replicate the macOS sequence (open-ended, several days)

- Find out what macOS does so that the CIO mailbox responds. Approaches:
  - Call `_PS0` only after the Thunderbolt driver has brought up the controller.
  - Set the `RTBT`/`RUSB` flags before suspend so that `PCDA`/`POFF` take effect as under macOS.
- Reverse engineering without documentation, only if the simple approach turns out to be a dead end.

## Measurements (kernel resume from S3 wakeup to "Restarting tasks")

| Setting | Duration |
|---|---|
| Thunderbolt loaded, `pm_async` off | 22.4 s |
| Thunderbolt loaded, `pm_async` on | 18.4 s |
| Thunderbolt unloaded, `pm_async` off | 16.0 s |
| bridges unbound, Thunderbolt loaded, `pm_async` on (with ACPI tracing) | ~6.4 s |
| bridges unbound, Thunderbolt unloaded, `pm_async` on (with ACPI tracing) | 1.54 s |
| sleep hook (bridges unbound + Thunderbolt unloaded), `pm_async` on | 1.51 s |
| additionally with hybrid sleep: slow CPUs | +16–36 s |

### First boot with the patched kernel (2026-10-26, generation 45): no GUI

- Kernel contains the quirk (`quirk_apple_titan_ridge_keep_d0` in `System.map`). It is a `FIXUP_SUSPEND` and does not run at boot.
- Actual failure: the greeter's `kwin_wayland` started at 27.4 s while `amdgpu` was still probing (until 32.2 s),
  got `Failed to open drm node: "/dev/dri/card1"` (i915), then `No suitable DRM devices` and exited. Nobody restarted it, so the screen stayed black.
- `EDID err: 2 on connector eDP-2` / `No EDID read` (`amdgpu 0000:03:00.0`) is a side effect: gmux routes the panel to the iGPU
  (`force_igd`), the dGPU's EDID read times out (~5.7 s) and makes the amdgpu probe slower (~9 s instead of ~4 s).
  Not seen in earlier boots, though; only one boot with the new kernel so far.
- Good boots: kwin starts ~1 s after amdgpu has finished. So this looks like a pre-existing race made visible by the slower probe, not the patch.
- Next: boot generation 45 again. If it happens again, compare with the old kernel (gen 44) and fix the greeter race
  (e.g. start the display manager only after the DRM drivers are loaded).
- Second boot of generation 45 (2026-10-26): GUI works. No EDID error; kwin started after amdgpu was ready (34.2 s → 35.5 s).
  Confirms the race theory. If it shows up again: make the greeter wait for the DRM drivers.

### Standby test with the patched kernel (2026-10-26, generation 45, sleep hook disabled)

- First two attempts (at 152 s and 173 s uptime) never reached S3: `Freezing user space processes failed … 1 tasks refusing to freeze`,
  the task was a `udev-worker` stuck in `applesmc_probe`/`applesmc_init`. udev loads `applesmc` twice per boot, each attempt
  hangs ~55 s and fails with -5 (until ~230 s uptime). Pre-existing, visible in all earlier boots. Fix: `boot.blacklistedKernelModules = [ "applesmc" ]`
  in `macbook-t2/boot.nix` (not yet active, needs rebuild + reboot). The T2 SMC is handled by `macsmc_*`.
- Third attempt (415 s uptime): S3 reached. The quirk fired for all four devices (`08:00.0`, `7e:00.0`, `06:00.0`, `7c:00.0`).
- **noirq resume 1.01 s, early 4 ms, resume 0.45 s, total ~1.5 s** (without patch 18.4 s; sleep hook 1.51 s), with `thunderbolt` loaded.
- NHI `pci_pm_resume_noirq`: 4 ms and 2 ms (before: ~5.8 s each). Largest remainder: `00:1c.0` (Wi-Fi root port) 1.01 s.
- After wakeup: bridges in D0 bound to `pcieport`, NHIs in D0 bound to `thunderbolt`, `domain0`/`domain1` present, no Thunderbolt errors.
  xHCI behind the bridges: "xHC error in resume … Reinit" as before.
- Still to check: USB-C/charging/phone after wakeup, several cycles, power draw in S3 overnight, a real Thunderbolt device.

## Wi-Fi dead after resume (brcmfmac firmware hang, analysis on 2026-10-28)

Symptom: After a standby on 2026-10-28 (S3 from 20:07:41 to 20:07:47, so only ~6 s), Wi-Fi did not come back.
Toggling Wi-Fi via rfkill did not help; only a reboot fixed it.

Log sequence (previous boot, `journalctl -b -1 -k`):
- From 20:07:49, directly after `PM: suspend exit`: `brcmf_msgbuf_query_dcmd: Timeout on response for query command`,
  errors -5 for `get_channel`, `get_tx_power`, `set_multicast_list`, `escan`. The firmware does not answer anymore.
- From 20:16: `brcmf_msgbuf_tx_ioctl: Failed to reserve space in commonring`, so the ring to the firmware is full and every
  call fails with -12 (ENOMEM). wpa_supplicant shows `CTRL-EVENT-SCAN-FAILED ret=-12`, NetworkManager shows
  `Nicht genügend Hauptspeicher verfügbar` when setting the MAC address. Misleading: it is not RAM.

Cause: the hot-resume path in `brcmf_pcie_pm_leave_D3()` (`drivers/net/wireless/broadcom/brcm80211/brcmfmac/pcie.c`):
- If `intmask` is still non-zero after resume, the driver assumes the firmware is still running, sends
  `BRCMF_H2D_HOST_D0_INFORM` and continues ("Hot resume"). This send does not fail, but the firmware is hung afterwards.
- Otherwise (`cleanup`), it detaches the chip and calls `brcmf_pcie_probe()`, which reloads the firmware. This path works.
- Boot from 2026-10-27 shows both paths alternating: one resume breaks Wi-Fi (phyN with timeouts), the next one reloads
  the firmware (`brcmf_c_preinit_dcmds: Firmware: BCM4364/4 …`, new phyN+1), and it works again.
- The broken resumes were all short (5–6 s in S3), the working ones mostly longer (20+ s). Plausible that the chip only loses
  its state in longer sleeps, but with ~5 cases this is a pattern, not proof (boot of 2026-10-26 had a 9 s sleep without errors).

Not the cause: kernel, kernel modules, firmware, kernel parameters and `modprobe.d` are identical between the working boots
(2026-10-25/26, nixpkgs `20261021.da6dc0e`) and the broken ones (`20261026.718f670`). Neither the flake update nor
patch 9006 / removing the Thunderbolt sleep hook changed anything for brcmfmac.

Immediate fix without reboot: `sudo modprobe -r brcmfmac_wcc brcmfmac && sudo modprobe brcmfmac`

### Fix: sleep hook for brcmfmac (2026-10-28)

- `macbook-t2/sleep.nix`: `powerManagement.powerDownCommands` runs `modprobe -r brcmfmac_wcc brcmfmac`,
  `powerManagement.resumeCommands` runs `modprobe brcmfmac`. So the firmware always starts fresh after a resume.
  (`brcmfmac_wcc` depends on `brcmfmac` and has to be removed first.)
- Build succeeds; both commands are present in `sleep-actions.service` (start / pre-stop script).
- **Not yet done: `nixos-rebuild switch`, test (short standby of a few seconds, then check that Wi-Fi reconnects), commit.**
- Cost: Wi-Fi reconnects ~1–2 s later after each resume; `wlp5s0` disappears and reappears, so NetworkManager sees a new device.

### Alternative: kernel patch (only if the hook causes problems)

- Simple (3–5 lines): skip the hot-resume branch in `brcmf_pcie_pm_leave_D3()` and always go to `cleanup`, limited to
  BCM4364 (`devinfo->ci->chip == BRCM_CC_4364_CHIP_ID`) or via a module parameter. Same effect as the hook, but `wlp5s0`
  stays. Would go into `macbook-t2/t2-patches/`; the kernel is built locally anyway, the code region rarely changes upstream.
- "Smart" variant (keep hot resume, fall back only if the firmware hangs): `D0_INFORM` does not fail, so this would need a
  test ioctl after resume with a timeout (up to 2 s per resume) and own testing. Not worth it.
- Decision: keep the hook for now (simpler, no patch to maintain).
