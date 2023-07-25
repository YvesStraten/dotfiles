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

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-hyprland
    ];
  };

  #    services.greetd = {
  #   enable = true;
  #   settings = rec {
  #     initial_session = {
  #       command = "${pkgs.hyprland}/bin/Hyprland";
  #       user = "yvess";
  #     };
  #     default_session = initial_session;
  #   };
  # };
}
