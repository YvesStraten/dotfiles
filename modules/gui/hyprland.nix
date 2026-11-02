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
        greetd = {
          enable = true;
          settings =
            let
              default = {
                command = "${lib.getExe pkgs.uwsm} start -eD Hyprland hyprland.desktop";
                user = "yvess";
              };
            in
            {
              initial_session = default;
              default_session = default;
            };
        };
      };

      security.pam.services = {
        # services.hyprlock = { };
        greetd.enableGnomeKeyring = true;
        login.enableGnomeKeyring = true;
      };
    };
}
