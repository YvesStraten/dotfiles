{ config
, options
, lib
, ...
}:
let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.custom.ghostty;
in
{
  options.custom.ghostty.enable = mkEnableOption "Enable ghostty";

  config = mkIf cfg.enable {
    programs.ghostty.enable = true;
  };
}
