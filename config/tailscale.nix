{
  config,
  options,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.custom.tailscale;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.tailscale.enable = mkEnableOption "Enable tailscale";

  config = mkMerge [
    (mkIf cfg.enable {
      environment.systemPackages = [ pkgs.tailscale ];

      services.tailscale = {
        enable = true;
        openFirewall = true;
      };
    })
  ];
}
