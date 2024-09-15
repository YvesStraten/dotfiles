{pkgs, ...}: {
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
}
