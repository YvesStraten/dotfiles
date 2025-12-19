{
  config,
  options,
  pkgs,
  gitUser,
  email,
  lib,
  ...
}:
let
  cfg = config.custom.git;
  inherit (lib) mkMerge mkEnableOption mkIf;
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
          email = email;
        };
      };

      lfs.enable = true;
    };
  };
}
