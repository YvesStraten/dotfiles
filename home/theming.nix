{pkgs, ...}: {
#  qt = {
#    enable = true;
#    platformTheme = "gtk";
#    style = {
#      name = "adwaita-dark";
#      package = pkgs.adwaita-qt;
#    };
#  };
#
#  gtk = {
#    enable = true;
#    gtk3.bookmarks = [
#      "file:///home/yvess/Gdrive/Uni"
#      "file:///home/yvess/Gdrive/Docs"
#      "file:///home/yvess/org"
#    ];
#    theme = {
#      name = "Catppuccin-Mocha-Compact-Pink-Dark";
#      package = pkgs.catppuccin-gtk.override {
#        accents = ["pink"];
#        size = "compact";
#        variant = "mocha";
#      };
#    };
#    iconTheme = {
#      name = "WhiteSur-dark";
#      package = pkgs.whitesur-icon-theme;
#    };
#  };

  home.packages = with pkgs; [
    #fonts
    (nerdfonts.override {
      fonts = [
        "JetBrainsMono"
      ];
    })
    ubuntu_font_family
    emacs-all-the-icons-fonts
  ];

  home.file.".local/share/fonts" = {
    source = ./fonts;
    recursive = true;
  };
}
