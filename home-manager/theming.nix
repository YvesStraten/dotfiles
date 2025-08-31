{ config
, options
, lib
, pkgs
, ...
}:
let
  cfg = config.custom.theming;
  inherit (lib) mkIf mkEnableOption;
in
{
  options.custom.theming = {
    enable = mkEnableOption "Enable theming";
    gtk.enable = mkEnableOption "gtk" // { default = cfg.enable; };
    qt.enable = mkEnableOption "qt" // { default = cfg.enable; };
  };

  config = mkIf cfg.enable {
    qt = mkIf cfg.qt.enable {
      enable = true;
      platformTheme.name = "gtk";
      style = {
        name = "gtk2";
        package = pkgs.libsForQt5.breeze-qt5;
      };
    };

    gtk = mkIf cfg.gtk.enable {
      enable = true;
      gtk3.bookmarks =
        let
          inherit (lib.attrsets) filterAttrs;
          xdg-dirs = config.xdg.userDirs;
          xdg-dirs-filtered = filterAttrs (k: v: k != "publishShare") xdg-dirs;
          xdg-entries = builtins.filter (elem: builtins.isString elem) (builtins.attrValues xdg-dirs-filtered);
          toBookmark = name: "file://${name}";
        in
        [
          # From onedrive service in hosts/nixos/default.nix
          "file:///home/yvess/Onedrive"
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
      nerd-fonts.fira-code
      ubuntu_font_family
      emacs-all-the-icons-fonts
    ];
  };
}
