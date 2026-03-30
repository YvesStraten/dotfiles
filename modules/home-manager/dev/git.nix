{ lib, ... }:
{
  flake.homeModules.dev =
    {
      config,
      osConfig,
      pkgs,
      ...
    }:
    let
      cfg = config.custom.git;
      inherit (osConfig.custom.constants) gitUser email;
      inherit (lib) mkEnableOption mkIf;
    in
    {
      options.custom.git.enable = mkEnableOption "Enable git" // {
        default = true;
      };

      config = mkIf cfg.enable {
        home.packages = [
          pkgs.lazygit
        ];

        programs.git = {
          enable = true;
          settings = {
            user = {
              name = gitUser;
              inherit email;
            };
          };

          lfs.enable = true;
        };
      };
    };
}
