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
    ./gnome.nix
    ./nvidia.nix
    ./zfs.nix
    ./vfio.nix
    ./auth.nix
    ./boot.nix
    ./bluetooth.nix
    ./i3.nix
    ./networking.nix
    ./users.nix
    ./power.nix
    ./nvidia.nix
    ./nix-settings.nix
    ./virtualisation.nix
    ./file-sharing.nix
    ./sound.nix
    ./tailscale.nix
    ./fonts.nix
  ];

  config = mkMerge [
    {
      environment.systemPackages = with pkgs; [
        vim
      ];
    }
  ];
}
