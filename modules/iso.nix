{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./nixos/nvidia.nix
    ./nixos/pkgs.nix
    ./nixos/security.nix
    ./nixos/services.nix
    ./nixos/settings.nix
    ./nixos/sound.nix
    ./nixos/time.nix
    ./nixos/vfio.nix

    # Desktops
    ../home/hypr/hyprland.nix
    # ../home/sway/sway.nix
    # ../home/gnome/gnome.nix
  ];

  isoImage.squashfsCompression = "gzip -Xcompression-level 1";
  boot = {
    kernelPackages = pkgs.linuxPackages_zen;
    supportedFilesystems = lib.mkForce ["btrfs" "reiserfs" "vfat" "f2fs" "xfs" "ntfs" "cifs"];
  };
}
