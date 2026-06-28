{
  self,
  lib,
  inputs,
  ...
}:
{
  flake.nixosModules.kde =
    { pkgs, config, ... }:
    {
      services = {
        # Enable Plasma
        desktopManager.plasma6.enable = true;

        displayManager.plasma-login-manager.enable = true;

        # Optionally enable xserver
        xserver.enable = true;
      };

      environment.systemPackages = [
        (pkgs.kdePackages.spectacle.override {
          tesseractLanguages = [
            "all"
          ];
        })
      ];
    };
}
