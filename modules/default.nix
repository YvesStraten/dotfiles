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

    # Desktops
    # ../home/hypr/hyprland.nix
    # ../home/sway/sway.nix
    ../home/gnome/gnome.nix
  ];
}
