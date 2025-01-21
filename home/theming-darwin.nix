{pkgs, ...}: {
  home.packages = with pkgs; [
    #fonts
    nerd-fonts.jetbrains-mono
    ubuntu_font_family
    emacs-all-the-icons-fonts
  ];
}
