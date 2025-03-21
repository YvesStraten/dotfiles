{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  cfg = config.custom.theming;
  inherit (lib) mkIf mkEnableOption;
in {
  options.custom.theming = {
    enable = mkEnableOption "Enable theming";
    gtk.enable = mkEnableOption "gtk" // {default = cfg.enable;};
    qt.enable = mkEnableOption "qt" // {default = cfg.enable;};
  };

  config = mkIf cfg.enable {
    qt = mkIf cfg.qt.enable {
      enable = true;
      platformTheme = "gtk";
      style = {
        name = "adwaita-dark";
        package = pkgs.adwaita-qt;
      };
    };

    gtk = mkIf cfg.gtk.enable {
      enable = true;
      gtk3.bookmarks = let
        xdg-dirs = config.xdg.userDirs;
        xdg-entries = builtins.filter (elem: builtins.isString elem) (builtins.attrValues xdg-dirs);
        toBookmark = name: "file://${name}";
      in
        [
          "file:///home/yvess/Gdrive/Uni"
          "file:///home/yvess/Gdrive/Docs"
          "file:///home/yvess/Notes"
        ]
        ++ (builtins.map toBookmark xdg-entries);
      theme = {
        name = "catppuccin-frappe-blue-standard";
        package = pkgs.catppuccin-gtk;
      };

      iconTheme = {
        name = "WhiteSur-dark";
        package = pkgs.whitesur-icon-theme;
      };

      cursorTheme = {
        package = pkgs.bibata-cursors;
        name = "Bibata-Modern-Ice";
      };
    };

    home.packages = with pkgs; [
      #fonts
      nerd-fonts.jetbrains-mono
      ubuntu_font_family
      emacs-all-the-icons-fonts
    ];
  };
}
