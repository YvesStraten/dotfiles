{
  config,
  options,
  lib,
  ...
}:
let
  cfg = config.custom.starship;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.starship.enable = mkEnableOption "Enable starship";

  config = mkIf cfg.enable {
    programs.starship = {
      enable = true;
      enableFishIntegration = true;
      enableZshIntegration = true;
      settings = {
        add_newline = false;
        hostname = {
          ssh_only = false;
          disabled = false;
        };
      };
    };
  };
}
