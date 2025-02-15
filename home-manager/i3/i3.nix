{
  options,
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.custom.i3;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.i3.enable = mkEnableOption "Enable i3";

  config = mkMerge [
    (mkIf cfg.enable {
      custom = {
        dunst.enable = true;
        rofi.enable = true;
        polybar.enable = true;
        picom.enable = true;
        udisks.enable = true;
      };

      services = {
        kdeconnect = {
          enable = true;
          indicator = true;
        };
      };

      # Required to get targets such as tray to work
      xsession.enable = true;

      home.file.".config/i3/config" = {
        source = ./config;
      };
    })
  ];
}
