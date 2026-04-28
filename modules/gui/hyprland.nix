{
  self,
  lib,
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

      security.pam = {
        # services.hyprlock = { };
        services.greetd.enableGnomeKeyring = true;
      };
    };
}
