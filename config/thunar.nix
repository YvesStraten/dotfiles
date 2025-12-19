{
  config,
  options,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.custom.thunar;

in
{
  options.custom.thunar.enable = mkEnableOption "Enable Thunar";

  config = mkIf cfg.enable {
    programs = {
      thunar = {
        enable = true;
        plugins = with pkgs.xfce; [
          thunar-archive-plugin
          thunar-volman
        ];
      };

      xfconf.enable = true;
    };

    services = {
      gvfs.enable = true;
      tumbler.enable = true;
    };

    environment.systemPackages = with pkgs; [
      ntfs3g
      file-roller
    ];
  };
}
