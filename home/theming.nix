{ pkgs, ... }:
let
  cursor = pkgs.stdenv.mkDerivation
    {
      name = "whitesur-cursors";
      src = pkgs.fetchFromGitHub {
        owner = "vinceliuice";
        repo = "WhiteSur-cursors";
        rev = "5c94e8c22de067282f4cf6d782afd7b75cdd08c8";
        sha256 = "03828f21sgcmpldbmqwpqbfvxrxy2zr9laipb27yy9kkfv8iwnq8";
      };
      installPhase = ''
        mkdir -p $out
        cp -R dist/* $out
      '';
    };
in
{
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
    theme = {
      name = "Catppuccin-Mocha-Compact-Pink-Dark";
      package = pkgs.catppuccin-gtk.override {
        accents = [ "pink" ];
        size = "compact";
        variant = "mocha";
      };
    };
    iconTheme = {
      name = "WhiteSur-dark";
      package = pkgs.whitesur-icon-theme;
    };
    font = {
      name = "Cantarell Regular";
      package = pkgs.cantarell-fonts;
      size = 11;
    };
    cursorTheme = {
      name = "WhiteSur-cursors";
      size = 24;
      package = cursor;
    };
    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = 1;
    };
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = 1;
    };
  };

  home.packages = with pkgs; [
    #fonts
    (nerdfonts.override {
      fonts = [
        "JetBrainsMono"
      ];
    })
    ubuntu_font_family
    whitesur-icon-theme
    catppuccin-gtk
    emacs-all-the-icons-fonts
  ];
}
