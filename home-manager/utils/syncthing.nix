{
  config,
  options,
  lib,
  ...
}:
let
  cfg = config.custom.syncthing;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.syncthing.enable = mkEnableOption "Enable syncthing";

  config = mkMerge [
    (mkIf cfg.enable {
      services.syncthing = {
        enable = true;
        tray = true;
      };
    })
  ];
}
