{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  packages = [
    pkgs.pkgsCross.mips-linux-gnu.buildPackages.gcc
    pkgs.pkgsCross.mips-linux-gnu.buildPackages.binutils
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
}
