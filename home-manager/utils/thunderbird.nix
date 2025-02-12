{
  config,
  options,
  lib,
  user,
  email,
  pkgs,
  ...
}:
let
  cfg = config.custom.thunderbird;
  inherit (lib) mkMerge mkEnableOption mkIf;
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
      profiles.${user} = {
        isDefault = true;
      };
    };
  };
}
