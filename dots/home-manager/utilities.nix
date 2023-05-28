{ pkgs, ...}:
{
  home.packages = with pkgs; [
    tesseract
    #texlive.combined.scheme-full
    zathura
    libreoffice
    rclone
    rclone-browser
    pandoc
    ranger
    ani-cli
    gnome.nautilus
    gwenview

    brave
    gh
    mpv
    ani-cli
  ];
}
