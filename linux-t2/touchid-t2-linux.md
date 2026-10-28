# Project: Touch ID (T2) on Linux on the MacBookPro16,1

- Date: 2026-10-25

## Goal

Use the Touch ID sensor on Linux, for `sudo` (PAM) and the KDE login (`plasmalogin`, screen lock `kde`).
Known state: unsupported by t2linux, no driver, libfprint/fprintd do not know the sensor.

## Findings (summary)

- The sensor is wired directly to the Secure Enclave (SEP) in the T2. Fingerprint data and templates never leave the T2.
- From Linux there is **no Touch ID device visible**, neither via USB/HID nor via PCI.
- The path from host to T2 is the CDC-NCM network link (`05ac:8233`, `enp4s0f1u1`). **Confirmed (stage 1):** the T2 responds
  there over IPv6 link-local and runs Remote Service Discovery (RemoteXPC) on TCP 59602.
- **The T2 lists, among others, the services `com.apple.eos.BiometricKit` (`bkremoted`, TCP 49158) and `com.apple.eos.LASecureIO`
  (`LASecureIOd`, TCP 49155).** Touch ID runs over BiometricKit under macOS. Both send a JSON greeting immediately on connect.
  Open question: whether they accept real requests without the macOS counterpart (pairing/entitlement).

### Devices behind the T2 (`t2bce_vhci`, bus 1)

| Port | ID | Device | Driver |
|---|---|---|---|
| 1-1 | 05ac:8233 | Apple T2 Controller | `cdc_ncm` → `enp4s0f1u1` (MAC `ac:de:48:00:11:22`) |
| 1-2 | 05ac:8514 | FaceTime HD Camera | `uvcvideo` |
| 1-3 | 05ac:8262 | Ambient Light Sensor | `usbhid` / `hid-sensor-hub` |
| 1-4 | 05ac:8103 | Headset (buttons) | `hid-generic` |
| 1-5 | 05ac:0340 | Internal Keyboard / Trackpad | `hid-apple`, `hid-magicmouse` |
| 1-6 | 05ac:8302 | Touch Bar Display | `hid_appletb_kbd` |
| 1-7 | 05ac:8102 | Touch Bar Backlight | `hid_appletb_bl` |
| 1-8 | 05ac:8104 | "Apple" with no product name | `hid-sensor-hub`, Usage Page 0x20 / Usage 0x8A (orientation, probably lid angle). **Not Touch ID.** |

T2 PCI functions: `04:00.0` NVMe, `04:00.1` `t2bce_core`, `04:00.2` (0x1802, no driver), `04:00.3` `t2bce_audio`.

## Stages and effort

1. **Explore (~30 min, harmless):** bring up the T2 link with IPv6 link-local, check whether the T2 responds (ping, mDNS, open ports).
2. **Capture the protocol (days):** requires macOS on the device. Capture traffic on the T2 link while Touch ID is used,
   decode RemoteXPC (groundwork: `pymobiledevice3`).
3. **Reimplement (weeks to months, outcome open):** likely obstacles: pairing/host auth against the T2, enrollment in the SEP
   is tied to the macOS user, firmware updates can change everything. Finally a PAM module (the easy part).

Pragmatic alternatives: USB fingerprint reader with libfprint (`services.fprintd.enable`, `security.pam.services.<x>.fprintAuth`)
or a FIDO2 key with `pam_u2f` (`security.pam.u2f.enable`, `security.pam.services.<x>.u2fAuth`).

## Stage 1: Explore

Approach:
- Without `sudo`: temporary NetworkManager connection `t2-probe` on `enp4s0f1u1`, IPv4 off, IPv6 link-local only, no autoconnect.
  Undo: `nmcli con delete t2-probe`.
- Before: interface `UP`, but `addr_gen_mode=1` (no link-local address), NM state "disconnected".
- Tests: `ping -6 ff02::1%enp4s0f1u1` (who answers?), port scan of the T2 address (`nmap` via `nix shell`, TCP connect scan),
  mDNS/DNS-SD query.

### Result (2026-10-25)

**Link:**
- T2 address: `fe80::aede:48ff:fe33:4455` (MAC `ac:de:48:33:44:55`), host side `ac:de:48:00:11:22`. Ping ~2–6 ms.
- mDNS (`_services._dns-sd._udp`, `_remoted._tcp`): no response.
- TCP connect scan over all ports (`nmap -6 -sT -p-`, ~4.5 min): only **59602 reported open**. The service ports 49152–49178
  did not show up in the scan, but were reachable right afterwards (see below). Cause unclear (possibly only open after the
  RSD handshake, or a rate limit).

**Port 59602 = RemoteXPC / RSD:**
- The server speaks HTTP/2 and immediately sends SETTINGS + WINDOW_UPDATE (the RemoteXPC signature).
- Handshake with `pymobiledevice3` (`RemoteXPCConnection`, without Lockdown) works. Reported, among others:
  `ProductName "Bridge OS"`, `OSVersion 10.6`, `BuildVersion 23P6068`, `ProductType iBridge2,14`, `HWModel J152fAP`,
  `HardwarePlatform t8012`, `HasSEP true`, `MessagingProtocolVersion 7`, `DeviceSupportsLockdown false`.
- The full RSD response (device identifiers redacted) is in `rsd_info.json` next to this file.

**Services relevant to Touch ID (port, entitlement required by macOS):**
- `com.apple.eos.BiometricKit` — 49158 — `com.apple.private.bmk.remote.allow`, process `bkremoted`, `UsesRemoteXPC: false`.
- `com.apple.eos.LASecureIO` — 49155 — `com.apple.private.lasecureio.remote`, process `LASecureIOd` (LocalAuthentication).
- Alongside these, ~25 more bridgeOS services (NFC relay, CoreSpeech/"Hey Siri", video encoder, logging/diagnostics, time, update).
  Each service has an `Entitlement` field. Under macOS the T2 presumably checks whether the connecting process holds this
  entitlement (via Apple-signed code). Whether the services accept requests without that check is the next test point.

**Service greeting** (on a bare connect, nothing sent): both (`bkremoted`, `LASecureIOd`) send a small frame with
magic `92 b8 01 00` + length + JSON:
`{"MaxSupportedProtocolVersion":1,"OSBuild":"23P6068","BridgeXPCVersion":39,"ProcessName":"bkremoted"}` resp. `"LASecureIOd"`.
This is a separate "BridgeXPC" frame (not the RSD HTTP/2). The protocol beyond this point is undocumented.

## Conclusion, stage 1

- The transport path exists and is usable from Linux **without pairing**: IPv6 link-local → RSD (59602) → service ports.
- The service responsible for Touch ID (`BiometricKit`) is running and reachable. That is considerably more than expected — the
  known state "no access at all" does not hold for the transport layer.
- **The real obstacle remains** the T2's entitlement/auth check and the undocumented BridgeXPC payload protocol above the
  greeting. Whether BiometricKit answers requests from a non-Apple-signed client is unclear and the crux for everything further.

## Next steps (optional, stage 2)

1. Pick a harmless service as a feasibility test (e.g. `com.apple.multiverse.remote.bridgetime`), determine the BridgeXPC payload
   format beyond the greeting (length + bplist/JSON?), send a request, and see whether the T2 responds without an entitlement.
   This settles the auth question without touching biometric functions.
2. Only if 1 works out: look at `BiometricKit` messages. That requires a capture under macOS on this device
   (traffic on the T2 link while using Touch ID) to learn the message format. Pure black-box attempts against the SEP
   are not sensible and carry the risk of tripping counters/lockouts.
