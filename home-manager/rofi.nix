{
  config,
  options,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.custom.rofi;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.rofi.enable = mkEnableOption "Enable rofi";

  config = mkIf cfg.enable {
    programs.rofi = {
      enable = true;
      package = pkgs.rofi-wayland;
      plugins = [ pkgs.rofi-emoji ];
      terminal = "${lib.getExe pkgs.kitty}";
    };
  };
}
