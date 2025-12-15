{
  config,
  options,
  lib,
  ...
}:
let
  cfg = config.custom.kde-connect;
  inherit (lib)
    mkMerge
    mkEnableOption
    mkIf
    mkForce
    ;
in
{
  options.custom.kde-connect.enable = mkEnableOption "Enable kde-connect";

  config = mkIf cfg.enable (mkMerge [
    {
      services.kdeconnect = {
        enable = true;
        indicator = true;
      };
    }

    {
      systemd.user.services.kdeconnect-indicator = {
        Unit.After = mkForce [ "graphical-session.target" ];
      };
    }
  ]);
}
