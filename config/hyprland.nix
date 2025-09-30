{
  config,
  options,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.hm.custom.hyprland;
  inherit (lib) mkEnableOption mkIf mkForce;
  hyprpkgs = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system};
in
mkIf cfg.enable {
  programs.uwsm.enable = true;
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
    withUWSM = true;
    package = hyprpkgs.hyprland;
    portalPackage = hyprpkgs.xdg-desktop-portal-hyprland;
  };

  programs.gamemode.settings.custom =
    let
      hyprctl = "${hyprpkgs.hyprland}/bin/hyprctl";
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

  services = {
    gnome.gnome-keyring.enable = true;
    greetd = {
      enable = true;
      settings =
        let
          default = {
            command = "${lib.getExe pkgs.uwsm} start -S hyprland-uwsm.desktop";
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
    services.hyprlock = { };
    services.greetd.enableGnomeKeyring = true;
  };
}
