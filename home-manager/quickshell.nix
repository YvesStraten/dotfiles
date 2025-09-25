{ config
, options
, lib
, inputs
, pkgs
, ...
}:
let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.custom.quickshell;
in
{
  options.custom.quickshell.enable = mkEnableOption "Enable quickshell";

  config = mkIf cfg.enable {
    home.packages = [
      inputs.quickshell.packages.${pkgs.system}.default
    ];
  };
}
