{
  config,
  options,
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
      type = types.string;
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
        undocked = {
          outputs = [
            {
              criteria = cfg.laptopScreen;
              status = "enable";
            }

            {
              criteria = cfg.hdmiScreen;
              status = "disable";
            }
          ];
        };

        docked = {
          outputs = [
            {
              criteria = cfg.laptopScreen;
              status = "disable";
            }

            {
              criteria = cfg.hdmiScreen;
              status = "enable";
            }
          ];
        };
      };
    };
  };
}
