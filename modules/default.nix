{ config
, pkgs
, lib
, ...
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
    ./nixos/vfio.nix

    ../overlays/default.nix

    # Desktops
    # ../home/hypr/hyprland.nix
    ../home/plasma/plasma.nix
    # ../home/sway/sway.nix
    # ../home/gnome/gnome.nix
  ];
}
