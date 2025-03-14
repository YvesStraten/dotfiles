{
  config,
  options,
  lib,
  ...
}:
let
  cfg = config.custom.syncthing;
  inherit (lib) mkMerge mkEnableOption mkIf mkForce;
in
{
  options.custom.syncthing.enable = mkEnableOption "Enable syncthing";

  config = mkMerge [
    (mkIf cfg.enable {
      services.syncthing = {
        enable = true;
        tray = true;
      };

      systemd.user.services.syncthingtray = {
        Unit.After = mkForce ["graphical-session.target" "tray.target" ];
      };
    })
  ];
}
