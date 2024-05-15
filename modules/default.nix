{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./nixos/bootloader.nix
    ./nixos/networking.nix
    ./nixos/nvidia.nix
    ./nixos/pkgs.nix
    ./nixos/security.nix
    ./nixos/services.nix
    ./nixos/settings.nix
    ./nixos/sound.nix
    ./nixos/time.nix

    ../overlays/default.nix

    # Desktops
    # ../home/hypr/hyprland.nix
    # ../home/i3/i3.nix
    # ../home/plasma/plasma.nix
    # ../home/sway/sway.nix
    ../home/gnome/gnome.nix
  ];

  fileSystems = {
    "/data".device = "/dev/nvme0n1p5";

  };
}
