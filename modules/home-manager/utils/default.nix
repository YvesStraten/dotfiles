{ lib, ... }:
{
  flake.homeModules.utils =
    {
      config,
      ...
    }:
    let
      cfg = config.custom.utils;
      inherit (lib) mkEnableOption mkIf mkMerge;
    in
    {
      options.custom.utils.enable = mkEnableOption "Enable utils" // {
        default = true;
      };

      config = mkMerge [
        (mkIf cfg.enable {
          custom = {
            yazi.enable = true;
            firefox = {
              enable = true;
              enablePwas = true;
            };
            zathura.enable = true;
            thunderbird.enable = true;
            syncthing.enable = true;
            general.enable = true;
          };

          programs.nh.enable = true;
        })
      ];
    };
}
