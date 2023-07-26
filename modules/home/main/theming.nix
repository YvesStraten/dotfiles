{pkgs, ...}: {
  home.packages = with pkgs; [
    #fonts
    (nerdfonts.override {
      fonts = [
        "JetBrainsMono"
      ];
    })

    #themes here
    (catppuccin-gtk.override {
      variant = "mocha";
    })
    whitesur-gtk-theme
    whitesur-icon-theme
  ];
}
