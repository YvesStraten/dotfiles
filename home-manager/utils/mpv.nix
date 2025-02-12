{
  config,
  options,
  lib,
  ...
}:
let
  cfg = config.custom.mpv;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.mpv.enable = mkEnableOption "Enable mpv";

  config = mkIf cfg.enable {
    programs.mpv = {
      enable = true;
      config = {
        sub-auto = "fuzzy";
        sub-font = "RobotoMono";
        sub-bold = true;
        tscale = "oversample";
        interpolation = true;
        save-position-on-quit = true;
      };
      defaultProfiles = [
        "gpu-hq"
      ];
    };
  };
}
