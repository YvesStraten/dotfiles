{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./nixos/bootloader.nix
    ./nixos/networking.nix
    ./nixos/pkgs.nix
    ./nixos/security.nix
    ./nixos/services.nix
    ./nixos/settings.nix
    ./nixos/sound.nix
    ./nixos/time.nix
    ./nixos/zfs.nix
    ./nixos/nvidia.nix

    ../overlays/default.nix

    # Desktops
    ../home/hypr/hyprland.nix
    # ../home/i3/i3.nix
    # ../home/plasma/plasma.nix
    # ../home/gnome/gnome.nix
  ];
}
