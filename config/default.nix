{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkMerge;
in
{
  imports = [
    ./hyprland.nix
    ./thunar.nix
    ./gnome.nix
    ./nvidia.nix
    ./zfs.nix
    ./vfio.nix
    ./auth.nix
    ./boot.nix
    ./bluetooth.nix
    ./networking.nix
    ./users.nix
    ./power.nix
    ./nvidia.nix
    ./design.nix
    ./nix-settings.nix
    ./virtualisation.nix
    ./file-sharing.nix
    ./sound.nix
    ./tailscale.nix
    ./fonts.nix
    ./kde.nix
  ];

  config = mkMerge [
    {
      environment.systemPackages = with pkgs; [
        unrar
        vim
      ];
    }
  ];
}
