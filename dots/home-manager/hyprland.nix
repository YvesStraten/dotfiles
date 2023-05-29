{ config, pkgs, ... }:
{
  home = {
    sessionVariables = {
      EDITOR = "nvim";
      TERMINAL = "kitty";
      _JAVA_AWT_WM_NONREPARENTING = 1;
      XCURSOR_SIZE=24;
      LIBVA_DRIVER_NAME = "nvidia";
      XDG_SESSION_TYPE = "wayland";
      GBM_BACKEND = "nvidia-drm";
      __GLX_VENDOR_LIBRARY_NAME = "nvidia";
      WLR_NO_HARDWARE_CURSORS = 1;
    };

  };

    programs.waybar = {
        enable = true;
        package = pkgs.waybar.overrideAttrs (oa: {
            mesonFlags = (oa.mesonFlags or []) ++ ["-Dexperimental=true"];
          });
      };

  }
