{
  config,
  pkgs,
  lib,
  ...
}: {
  services.xserver = {
    desktopManager = {
      xterm.enable = false;
    };

    displayManager = {
      sddm.enable = true;
      defaultSession = "none+bspwm";
    };

    windowManager.bspwm = {
      enable = true;
      configFile = "/home/yvess/.config/bspwm/bspwmrc";
    };
  };
}
