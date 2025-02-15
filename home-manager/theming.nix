{
  config,
  options,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.custom.theming;
  inherit (lib) mkIf mkEnableOption;
in
{
  options.custom.theming.enable = mkEnableOption "Enable theming";

  config = mkIf cfg.enable {
    qt = {
      enable = true;
      platformTheme = "gtk";
      style = {
        name = "adwaita-dark";
        package = pkgs.adwaita-qt;
      };
    };

    gtk = {
      enable = true;
      gtk3.bookmarks = [
        "file:///home/yvess/Gdrive/Uni"
        "file:///home/yvess/Gdrive/Docs"
        "file:///home/yvess/Notes"
      ];
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
