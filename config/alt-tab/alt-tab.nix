{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkOption types mkIf platforms;
  inherit (lib.hm) assertions;
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
    assertions = [(assertions.assertPlatform "services.alt-tab" pkgs platforms.darwin)];
    home.packages = [
      pkgs.alt-tab-macos
    ];

    launchd.agents.alt-tab = {
      enable = true;
      config = {
        ProgramArguments = [
          "${pkgs.alt-tab-macos}/Applications/AltTab.app/Contents/MacOS/AltTab"
        ];
        Label = "org.nix-community.home.alt-tab";
        ProcessType = "Interactive";
        LegacyTimers = true;
        LimitLoadToSessionType = "Aqua";
        RunAtLoad = true;
      };
    };
  };
}
