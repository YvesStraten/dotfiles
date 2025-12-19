{
  config,
  options,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.custom.nwg-dock;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.nwg-dock = {
    enable = mkEnableOption "Enable nwg-dock";
  };

  config = mkIf cfg.enable {
    systemd.user.services.nwg-dock = {
      Install.WantedBy = [ "graphical-session.target" ];
      Unit = {
        Description = "Start nwg-dock";
        After = [ "graphical-session.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        Type = "simple";
        ExecStart = "${lib.getExe pkgs.nwg-dock-hyprland} -d";
      };
    };
  };
}
