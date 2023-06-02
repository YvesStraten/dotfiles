{pkgs, ...}: {
  home.packages = with pkgs; [
    tesseract
    zathura
    libreoffice
    rclone
    rclone-browser
    pandoc
    ranger
    ani-cli
    gnome.nautilus
    gnome.gnome-clocks
    gwenview
    themechanger
    spicetify-cli
    gscan2pdf

    gh
    git
    mpv
    ani-cli
    neofetch
  ];
}
