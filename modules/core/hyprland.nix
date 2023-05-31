{ config, pkgs, lib, ... }: {
  programs.hyprland = {
    enable = true;
    xwayland = {
      enable = true;
      hidpi = true;
    };
    nvidiaPatches = true;
  };

  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };
}
