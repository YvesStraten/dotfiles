{
  config,
  options,
  lib,
  ...
}:
let
  cfg = config.custom.bluetooth;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.bluetooth.enable = mkEnableOption "Bluetooth";

  config = mkIf cfg.enable {
    hardware.bluetooth.enable = true;
    services.blueman.enable = true;
  };
}
