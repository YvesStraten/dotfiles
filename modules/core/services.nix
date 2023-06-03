{
  config,
  pkgs,
  lib,
  ...
}
: {
  services.upower = {
    enable = true;
    criticalPowerAction = "Hibernate";
  };

  services.greetd = {
    enable = true;
    settings = rec {
      initial_session = {
        command = "${pkgs.hyprland}/bin/Hyprland";
        user = "yvess";
      };
      default_session = initial_session;
    };
  };

  services.flatpak.enable = true;

  services.printing = {
      enable = true;
      drivers = [
        pkgs.gutenprintBin
      ];
    };

  virtualisation.libvirtd.enable = true;
}
