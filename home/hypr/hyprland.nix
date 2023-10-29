{
  pkgs,
  lib,
  config,
  ...
}: {
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
    enableNvidiaPatches = true;
  };

  services.xserver.displayManager.sddm = {
    enable = true;
    theme = "${pkgs.yvess.sugar-dark}";
  };

  environment.systemPackages = with pkgs; [
    config.nur.repos.mikilio.xwaylandvideobridge-hypr
  ];

  sound.enable = false;
  security.rtkit.enable = true;
  security.polkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
}
