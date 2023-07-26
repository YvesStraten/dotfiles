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
      name = "Whitesur";
      package = pkgs.whitesur-icon-theme;
    };
    font = {
      name = "Cantarell Regular";
      size = 11;
    };
    cursorTheme = {
      name = "dark-sense";
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
