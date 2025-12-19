{
  config,
  options,
  lib,
  ...
}:
let
  cfg = config.custom.yazi;
  inherit (lib) mkEnableOption mkIf;
in
{
  options.custom.yazi.enable = mkEnableOption "Enable yazi";

  config = mkIf (cfg.enable || config.custom.hyprland.enable || config.custom.i3.enable) {
    programs.yazi = {
      enable = true;
      enableZshIntegration = true;
      enableFishIntegration = true;
    };

    home.file.".config/yazi/yazi.toml" = {
      source = ./config/yazi.toml;
    };

    home.file.".config/yazi/keymap.toml" = {
      source = ./config/keymap.toml;
    };
  };
}
