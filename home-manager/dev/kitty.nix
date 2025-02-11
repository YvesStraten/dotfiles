{
  config,
  options,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.custom.kitty;
  inherit (lib)
    mkOption
    mkEnableOption
    mkIf
    types
    ;
in
{
  options.custom.kitty = {
    enable = mkEnableOption "Enable kitty";
    withWindowDecorations = mkOption {
      type = types.bool;
      default = (if pkgs.stdenv.isDarwin then true else false);
      description = ''
        Enable window decorations
      '';
    };
  };

  config = mkIf cfg.enable {
    programs.kitty = {
      enable = true;
      font = {
        name = "FiraCode";
        size = 18;
      };

      shellIntegration = {
        enableZshIntegration = true;
        enableFishIntegration = true;
      };

      settings = {
        macos_quit_when_last_window_closed = true;
        titlebar-only = true;
        confirm_os_window_close = 0;
        background_opacity = 0.8;
        enable_audio_bell = false;
        hide_decorations = mkIf cfg.withWindowDecorations true;
      };
    };
  };
}
