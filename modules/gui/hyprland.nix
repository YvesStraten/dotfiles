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
          package = pkgs.hyprland;
          portalPackage = pkgs.xdg-desktop-portal-hyprland;
        };

        gamemode.settings.custom =
          let
            hyprctl = "${pkgs.hyprland}/bin/hyprctl";
            start = pkgs.writeShellScriptBin "start_gamemode" ''
                        ${hyprctl} --batch "\
                        keyword animations:enabled 0;\
                        keyword animation borderangle,0; \
                        keyword decoration:shadow:enabled 0;\
                        keyword decoration:blur:enabled 0;\
              	        keyword decoration:fullscreen_opacity 1;\
                        keyword general:gaps_in 0;\
                        keyword general:gaps_out 0;\
                        keyword general:border_size 1;\
                        keyword decoration:rounding 0"
                      ${lib.getExe pkgs.libnotify} "Enabled gamemode"
            '';

            end = pkgs.writeShellScriptBin "end_gamemode" ''
              ${hyprctl} reload
              ${lib.getExe pkgs.libnotify} "Disabled gamemode"
            '';
          in
          mkIf config.programs.gamemode.enable {
            start = "${lib.getExe start}";
            end = "${lib.getExe end}";
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
