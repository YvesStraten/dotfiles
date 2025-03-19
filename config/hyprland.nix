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
in
mkIf cfg.enable {
  programs.uwsm.enable = true;
  programs.hyprland =
    let
      hyprpkgs = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system};
    in
    {
      enable = true;
      xwayland.enable = true;
      withUWSM = true;
      package = hyprpkgs.hyprland;
      portalPackage = hyprpkgs.xdg-desktop-portal-hyprland;
    };

  security.pam.services.hyprlock = { };
  services.gnome.gnome-keyring.enable = true;

  services.greetd = {
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
}
