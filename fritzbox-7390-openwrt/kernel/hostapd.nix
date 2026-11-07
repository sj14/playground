# A small static MIPS build of hostapd for the initramfs. It uses hostapd's
# internal crypto instead of OpenSSL and leaves out the enterprise features.
# The size matters: EVA stopped responding while receiving a 5.9 MB image.
{ pkgs ? import <nixpkgs> { } }:

(pkgs.pkgsCross.mips-linux-gnu.pkgsStatic.hostapd.override {
  sqlite = null;
}).overrideAttrs (old: {
  extraConfig = ''
    CONFIG_DRIVER_NL80211=y
    CONFIG_LIBNL32=y
    CONFIG_ELOOP_EPOLL=y
    CONFIG_TLS=internal
    CONFIG_INTERNAL_LIBTOMMATH=y
    CONFIG_CRYPTO=internal
    CONFIG_NO_TKIP=y
    CONFIG_NO_RADIUS=y
    CONFIG_NO_ACCOUNTING=y
    CONFIG_NO_VLAN=y
    CONFIG_GETRANDOM=y
    CONFIG_IEEE80211N=y
    CONFIG_IEEE80211W=y
  '';

  # Setting these to "n" would not disable them, because the Makefile only
  # checks whether they are defined. They also need OpenSSL's elliptic curves.
  preBuild = (old.preBuild or "") + ''
    sed -i -E '/^CONFIG_(DPP|DPP2|DPP3|SAE|OWE|SUITEB192|EAP[A-Z0-9_]*|PKCS12|IEEE8021X_EAPOL)=/d' .config
  '';
})
