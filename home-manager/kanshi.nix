{
  config,
  options,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.custom.kanshi;
  inherit (lib)
    mkMerge
    mkEnableOption
    mkIf
    mkOption
    types
    ;
in
{
  options.custom.kanshi = {
    enable = mkEnableOption "Enable kanshi";
    laptopScreen = mkOption {
      type = types.str;
      default = "eDP-1";
      description = ''
        Your internal display monitor name, if any
      '';
    };

    hdmiScreen = mkOption {
      type = types.string;
      default = "HDMI-A-1";
      description = ''
        Your external display monitor name, if any
      '';
    };
  };

  config = mkIf cfg.enable {
    services.kanshi = {
      enable = true;
      profiles = {
        "machine_docked_home" = {
          outputs = [
            {
              criteria = cfg.laptopScreen;
              status = "disable";
            }

            {
              criteria = "BNQ BenQ EX2510 M8L08374019";
              status = "enable";
            }
          ];
        };

        "machine_docker_other" = {
          outputs = [
            {
              criteria = cfg.laptopScreen;
              status = "enable";
            }

            {
              criteria = cfg.hdmiScreen;
              status = "enable";
            }
          ];

          exec = ["${pkgs.hyprland}/bin/hyprctl keyword monitor ${cfg.hdmiScreen},highres,auto,1,mirror,${cfg.laptopScreen}"];
        };

        "machine_undocked" = {
          outputs = [
            {
              criteria = cfg.laptopScreen;
              status = "enable";
            }
          ];
        };
      };
    };
  };
}
