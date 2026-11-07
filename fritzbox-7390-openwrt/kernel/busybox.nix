# A small static MIPS busybox for the initramfs. Image size matters, see the
# README, so it only has what a shell over telnet needs.
{ pkgs ? import <nixpkgs> { } }:

pkgs.pkgsCross.mips-linux-gnu.pkgsStatic.busybox.override {
  enableMinimal = true;
  extraConfig = ''
    CONFIG_ASH y
    CONFIG_ASH_ECHO y
    CONFIG_ASH_TEST y
    CONFIG_ASH_PRINTF y
    CONFIG_SH_IS_ASH y
    CONFIG_BASH_IS_NONE y
    CONFIG_FEATURE_SH_STANDALONE n
    CONFIG_TELNETD y
    CONFIG_FEATURE_TELNETD_STANDALONE y
    CONFIG_FEATURE_TELNETD_PORT_DEFAULT 23
    CONFIG_FEATURE_TELNETD_INETD_WAIT n
    CONFIG_FEATURE_DEVPTS y
    CONFIG_CAT y
    CONFIG_DMESG y
    CONFIG_FREE y
    CONFIG_IFCONFIG y
    CONFIG_LS y
    CONFIG_MOUNT y
    CONFIG_PS y
    CONFIG_REBOOT y
    CONFIG_UNAME y
    CONFIG_UPTIME y
  '';
}
