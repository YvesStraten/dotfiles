{
  config,
  options,
  lib,
  ...
}:
let
  cfg = config.custom.zellij;
  inherit (lib) mkEnableOption mkIf;
in
{
  options.custom.zellij.enable = mkEnableOption "Enable zellij";

  config = mkIf cfg.enable {
    programs.zellij.enable = true;
  };
}
