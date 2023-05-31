{pkgs, ...}: {
  home.packages = with pkgs; [
    tesseract
    # texlive.combined.scheme-full
    zathura
    libreoffice
    rclone
    rclone-browser
    pandoc
    ranger
    ani-cli
    gnome.nautilus
    gwenview
    themechanger
    spicetify-cli
    gscan2pdf

    gh
    mpv
    ani-cli
  ];
}
