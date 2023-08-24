{ config
, pkgs
, ...
}: {
  home = {
    sessionVariables = {
      GRIMBLAST_EDITOR = "gwenview";
      _JAVA_AWT_WM_NONREPARENTING = "1";
      XCURSOR_SIZE = "24";
      WLR_NO_HARDWARE_CURSORS = "1";
      XDG_SESSION_TYPE = "wayland";
      NIXOS_OZONE_WL = "1";
      XDG_CURRENT_DESKTOP = "Hyprland";
      XDG_SESSION_DESKTOP = "Hyprland";
    };
  };

  home.file = {
    ".config/hypr/hyprland.conf" = {
      source = ./hyprland.conf;
    };

    ".config/hypr/scripts" = {
      source = ./scripts;
      recursive = true;
    };
  };

  services.udiskie = {
    enable = true;
    automount = true;
    tray = "never";
  };
}
