{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  inherit (lib) mkOption types mkIf;
  cfg = config.services.alt-tab;
in {
  options = {
    services.alt-tab = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enables the alt-tab service
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      pkgs.yvess.alt-tab
    ];

    launchd.agents.alt-tab = {
      enable = true;
      config = {
        ProgramArguments = [
          "${pkgs.yvess.alt-tab}/Applications/AltTab.app/Contents/MacOS/AltTab"
        ];
        Label = "com.lwouis.alt-tab-macos";
        ProcessType = "Interactive";
        LegacyTimers = true;
        LimitLoadToSessionType = "Aqua";
        RunAtLoad = true;
        KeepAlive = true;
      };
    };
  };
}
