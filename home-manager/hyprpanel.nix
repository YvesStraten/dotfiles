{ config
, options
, inputs
, pkgs
, lib
, ...
}:
let
  cfg = config.custom.hyprpanel;
  inherit (lib)
    mkMerge
    mkEnableOption
    mkIf
    mkForce
    ;
in
{
  options.custom.hyprpanel.enable = mkEnableOption "Enable Hyprpanel";

  config = mkIf cfg.enable (mkMerge [
    {
      programs.hyprpanel = {
        enable = true;


        settings = {
          bar = {
            launcher.autoDetectIcon = true;
            workspaces.show_icons = true;
          };

          bar.layouts = {
            "*" = {
              left = [
                "dashboard"
                "workspaces"
                "windowtitle"
                "battery"
                "hypridle"
              ];
              middle = [ "media" ];
              right = [
                "volume"
                "network"
                "bluetooth"
                "systray"
                "clock"
                "notifications"
              ];

            };
            dashboard = {
              shortcuts =
                let
                  launch = app: "${lib.getExe pkgs.uwsm}/bin/uwsm app -- ${app}";
                in
                {
                  left = {
                    shortcut1 = {
                      command = launch "firefox";
                    };

                    shortcut2 = {
                      command = launch "spotify";
                    };

                    shortcut3 = {
                      command = launch "vesktop";
                    };
                  };
                };
            };
          };
        };
      };
    }
  ]);
}
