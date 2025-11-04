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
      type = types.str;
      default = "HDMI-A-1";
      description = ''
        Your external display monitor name, if any
      '';
    };
  };

  config = mkIf cfg.enable {
    services.kanshi = {
      enable = true;
      settings = [
        {
          output.criteria = cfg.laptopScreen;
        }

        {
          profile = {
            name = "machine_docked_home";

            outputs = [
              {
                criteria = cfg.laptopScreen;
                status = "enable";
                position = "1600,0";
              }

              {
                criteria = "BNQ BenQ EX2510 M8L08374019";
                status = "enable";
                mode = "1920x1080@144";
                position = "0,0";
              }
            ];
          };
        }

        {
          profile = {
            name = "machine_docked_other";
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
          };
        }
      ];
    };
  };
}
