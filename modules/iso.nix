{ config
, pkgs
, lib
, ...
}: {
  imports = [
    ./nixos/nvidia.nix
    ./nixos/pkgs.nix
    ./nixos/security.nix
    ./nixos/services.nix
    ./nixos/settings.nix
    ./nixos/sound.nix
    ./nixos/time.nix

    # Desktops
    # ./nixos/hyprland.nix
    # ./nixos/qtile.nix
    # ./nixos/i3.nix
    # ./nixos/sway.nix
    # ./nixos/bspwm.nix
    ./nixos/plasma.nix
  ];
}
