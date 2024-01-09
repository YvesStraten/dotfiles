{
  config,
  pkgs,
  lib,
  ...
}: {
  services.xserver = {
    desktopManager.plasma5.enable = true;

    displayManager = {
      defaultSession = "plasmawayland";
      sddm = {
        enable = true;
      };
    };
  };

  programs.partition-manager.enable = true;
}
