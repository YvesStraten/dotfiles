{
  config,
  options,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.hm.custom.gnome;
  inherit (lib) mkMerge mkIf mkForce;
in
{
  config = mkIf cfg.enable (mkMerge [
    {
      services.power-profiles-daemon.enable = mkForce false;
    }

    {
      services.xserver = {
        displayManager.gdm.enable = true;
        desktopManager.gnome.enable = true;
      };

      environment.systemPackages = cfg.extensions;
      environment.gnome.excludePackages =
        with pkgs; [
          gnome-terminal
          gnome-photos
          gnome-tour
          gnome-music
          epiphany
          geary
          tali
          iagno
          hitori
          atomix
        ];
    }
  ]);
}
