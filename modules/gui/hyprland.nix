{
  self,
  lib,
  inputs,
  ...
}:
{
  flake.nixosModules.hyprland =
    { pkgs, config, ... }:
    let
      inherit (lib) mkIf;
    in
    {
      imports = [
        self.nixosModules.shikane
        self.nixosModules.kvantum
      ];

      environment.systemPackages =
        with pkgs;
        [
          nwg-displays
          kdePackages.dolphin
          grim
          slurp
          satty
        ]
        ++ [
          inputs.noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default
          inputs.vicinae.packages.${pkgs.stdenv.hostPlatform.system}.default
        ];

      xdg.portal.xdgOpenUsePortal = true;
      programs = {
        uwsm.enable = true;
        hyprland = {
          enable = true;
          xwayland.enable = true;
          withUWSM = true;
        };
      };

      services = {
        udisks2.enable = true;
        gnome.gnome-keyring.enable = true;

        displayManager.sddm = {
          enable = true;
          wayland.enable = true;
        };
      };

      security.pam.services = {
        login.enableGnomeKeyring = true;
      };
    };
}
