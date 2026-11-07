{ pkgs ? import <nixpkgs> { } }:

let
  mips = pkgs.pkgsCross.mips-linux-gnu;
in
pkgs.mkShell {
  packages = [
    mips.pkgsStatic.stdenv.cc
    mips.pkgsStatic.stdenv.cc.bintools
    pkgs.bc
    pkgs.bison
    pkgs.curl
    pkgs.flex
    pkgs.gnumake
    pkgs.gnupatch
    pkgs.perl
    pkgs.python3
    pkgs.xz
    pkgs.zig
  ];

  # A static MIPS build, which goes into the initramfs.
  HOSTAPD = "${import ./hostapd.nix { inherit pkgs; }}/bin/hostapd";

  # cfg80211 needs this database for 5 GHz.
  REGDB = "${pkgs.wireless-regdb}/lib/firmware";

  # A static MIPS build, which goes into the initramfs.
  BUSYBOX = "${import ./busybox.nix { inherit pkgs; }}/bin/busybox";
}
