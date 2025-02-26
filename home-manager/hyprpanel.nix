{
  config,
  options,
  inputs,
  pkgs,
  lib,
  ...
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
  imports = [
    inputs.hyprpanel.homeManagerModules.hyprpanel
  ];

  options.custom.hyprpanel.enable = mkEnableOption "Enable Hyprpanel";

  config = mkIf cfg.enable (mkMerge [
    {
      assertions = [
        {
          assertion = !config.services.dunst.enable && !config.services.mako.enable;
          message = ''
            Dunst and/or Mako are not compatible with Hyprpanel

            Please disable them by setting
            services.dunst.enable = false and
            services.mako.enable = false
          '';
        }
      ];
    }

    {
      programs.hyprpanel = {
        enable = true;
        overwrite.enable = true;
        settings = {
          menus = {
            dashboard = {
              shortcuts = {
                left = {

                  shortcut1 = {
                    command = "firefox";
                  };

                  shortcut2 = {
                    command = "spotify";
                  };

                  shortcut3 = {
                    command = "vesktop";
                  };
                };
              };
            };
          };
        };
      };

      systemd.user.services = {
        hyprpanel = {
          Install = {
            WantedBy = [ "graphical-session.target" ];
          };
          Unit = {
            Description = "Start hyprpanel";
            After = [ "graphical-session.target" ];
            PartOf = [ "graphical-session.target" ];
          };

          Service = {
            Type = "simple";
            ExecStart = "${lib.getExe pkgs.hyprpanel}";
            ExecReload = "kill -SIGUSR1 $MAINPID";
          };
        };
      };
    }
  ]);
}
