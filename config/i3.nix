{
  config,
  options,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.hm.custom.i3;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
mkIf cfg.enable {
  environment.pathsToLink = [ "/libexec" ];
  services.xserver = {
    enable = true;

    desktopManager = {
      xterm.enable = false;
    };

    displayManager = {
      defaultSession = "none+i3";
    };

    windowManager.i3 = {
      package = pkgs.i3-gaps;
      enable = true;
      extraPackages = with pkgs; [
        autotiling
        tdrop
        pamixer
        pulseaudio
        feh
        pywal
        picom
        maim
        autorandr
        xdotool
        xclip
      ];
    };
  };

}
