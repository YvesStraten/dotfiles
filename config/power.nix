{
  config,
  options,
  lib,
  ...
}:
let
  cfg = config.custom.power;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.power.enable = mkEnableOption "Enable laptop power settings" // {
    default = true;
  };

  config = mkIf cfg.enable {
    services = {
      thermald.enable = true;
      tlp.enable = true;
      upower = {
        enable = true;
        criticalPowerAction = "Hibernate";
      };

      logind.extraConfig = ''
        HandlePowerKey=ignore
      '';
    };
  };

}
