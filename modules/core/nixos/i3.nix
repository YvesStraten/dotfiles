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
      defaultSession = "none+i3";
      autoLogin.enable = true;
      autoLogin.user = "yvess";
    };

    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-rounded;
    };
  };
}
