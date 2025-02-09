{
  config,
  options,
  lib,
  ...
}:
let
  cfg = config.custom.udisks;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.udisks.enable = mkEnableOption "Enable udiskie";

  config = mkIf cfg.enable {
    udiskie = {
      enable = true;
      automount = true;
      tray = "never";
    };
  };
}
