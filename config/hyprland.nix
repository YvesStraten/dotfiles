{
  config,
  options,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.hm.custom.hyprland;
  inherit (lib) mkEnableOption mkIf;
in
mkIf cfg.enable {
  programs.uwsm.enable = true;
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
    withUWSM = true;
  };

  services.displayManager.sddm = {
    enable = true;
    theme = "${pkgs.yvess.sugar-dark}";
    wayland.enable = true;
  };

  security.rtkit.enable = true;
  security.polkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  security.pam.services.hyprlock = { };
}
