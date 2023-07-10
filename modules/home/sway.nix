{
  config,
  pkgs,
  ...
}: {
  home = {
    sessionVariables = {
      EDITOR = "nvim";
      TERMINAL = "kitty";
      GRIMBLAST_EDITOR = "gwenview";
      _JAVA_AWT_WM_NONREPARENTING = "1";
      # Hardware cursors not yet working on wlroots
      WLR_NO_HARDWARE_CURSORS = "1";

      # General wayland environment variables
      XDG_SESSION_TYPE = "wayland";
      QT_QPA_PLATFORM = "wayland";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";

      # Xwayland compatibility
      XWAYLAND_NO_GLAMOR = "1";
    };
  };

  /* programs.waybar = {
    enable = true;
    package = pkgs.waybar.overrideAttrs (oa: {
      mesonFlags = (oa.mesonFlags or []) ++ ["-Dexperimental=true"];
    });
  }; */
}
