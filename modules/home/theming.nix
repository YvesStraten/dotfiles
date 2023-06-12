{pkgs, ...}: {
  home.packages = with pkgs; [
    #fonts
    (nerdfonts.override {
      fonts = [
        "JetBrainsMono"
        "FiraCode"
      ];
    })
    noto-fonts-emoji
    noto-fonts
    font-awesome 
    comic-mono

    #themes here
    (catppuccin-gtk.override {
      accents = ["pink"];
      variant = "mocha";
    })
    whitesur-gtk-theme
    whitesur-icon-theme
  ];
}
