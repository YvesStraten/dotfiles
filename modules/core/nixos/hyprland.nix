{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.hyprland = {
    enable = true;
    xwayland = {
      enable = true;
      hidpi = true;
    };
    nvidiaPatches = true;
  };

  services.udisks2.enable = true;

  services.xserver.displayManager = {
    defaultSession = "hyprland";
    
sddm = {
    enable = true;
  };
    
  };

  programs.nm-applet.enable = true;
}
