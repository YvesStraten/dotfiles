{ pkgs
, lib
, options
, config
, ...
}:
let
  cfg = config.custom.picom;
  inherit (lib) mkIf mkEnableOption;
in
{
  options.custom.picom.enable = mkEnableOption "Enable picom";

  config = mkIf cfg.enable {
    services.picom = {
      enable = true;
      package = pkgs.picom-pijulius;
    };

    xdg.configFile."picom/picom.conf" = {
      source = ./picom.conf;
    };
  };
}
