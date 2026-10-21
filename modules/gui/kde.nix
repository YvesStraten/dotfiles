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

      environment.systemPackages =
        with pkgs.kdePackages;
        [
          kdePackages.kio # needed since 25.11
          kdePackages.kio-fuse # to mount remote filesystems via FUSE
          kdePackages.kio-extras # extra protocols support (sftp, fish and more)
          kdePackages.dolphin
          kdePackages.kdegraphics-thumbnailers
          (kdePackages.spectacle.override {
            tesseractLanguages = [
              "all"
            ];
          })
        ]
        ++ [ pkgs.cifs-utils ];
    };
}
