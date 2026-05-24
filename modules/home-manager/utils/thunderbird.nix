{ lib, ... }:
{
  flake.homeModules.utils =
    {
      config,
      osConfig,
      pkgs,
      ...
    }:
    let
      cfg = config.custom.thunderbird;
      inherit (osConfig.custom.constants) email user;
      inherit (lib) mkEnableOption mkIf;
    in
    {
      options.custom.thunderbird.enable = mkEnableOption "Enable thunderbird" // {
        default = (if pkgs.stdenv.isLinux then true else false);
      };

      config = mkIf cfg.enable {
        accounts.email.accounts = {
          ${user} = {
            address = email;
            primary = true;
          };
        };

        programs.thunderbird = {
          enable = true;
          package = pkgs.thunderbird-140;
          profiles.${user} = {
            isDefault = true;
          };
        };
      };
    };
}
