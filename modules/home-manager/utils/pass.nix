{ lib, ... }:
{
  flake.homeModules.utils =
    {
      config,
      pkgs,
      ...
    }:
    let
      cfg = config.custom.pass;
      inherit (lib) mkEnableOption mkIf;
    in
    {
      options.custom.pass = {
        enable = mkEnableOption "Enable password store" // {
          default = true;
        };
        wayland = mkEnableOption "Using wayland";
        x = mkEnableOption "Using x";
      };

      config = mkIf cfg.enable {
        programs = {
          gpg.enable = true;
          browserpass.enable = true;
        };

        home.packages = with pkgs; [
          (mkIf cfg.wayland wl-clipboard)
          (mkIf cfg.x xclip)
        ];
      };
    };
}
