{ config
, options
, lib
, ...
}:
let
  cfg = config.custom.bluetooth;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.bluetooth.enable = mkEnableOption "Bluetooth" // { default = true; };

  config = mkIf cfg.enable {
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = false;
      settings = {
        General = {
          Experimental = true;
        };
      };
    };
    services.blueman.enable = true;
  };
}
