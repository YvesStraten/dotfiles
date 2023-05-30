{ pkgs, ...}:
{
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

    gh
    mpv
    ani-cli
  ];
}
