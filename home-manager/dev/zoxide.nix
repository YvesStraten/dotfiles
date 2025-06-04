{ config
, options
, lib
, ...
}:
let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.custom.zoxide;
in
{
  options.custom.zoxide.enable = mkEnableOption "Enable zoxide";

  config = mkIf cfg.enable {
    programs.zoxide = {
      enable = true;
      enableFishIntegration = true;
    };
  };
}
