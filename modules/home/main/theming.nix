{pkgs, ...}: {
  gtk = {
    enable = true;
    theme = {
      name = "Catppuccin-Mocha-Compact-Pink-Dark";
      package = pkgs.catppuccin-gtk.override {
        accents = ["pink"];
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
      name = "Nordzy-cursors";
      size = 24;
      package = pkgs.nordzy-cursor-theme;
    };
    gtk4.extraConfig = {
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
  ];
}
