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

    brave
    gh
    mpv
  ];
}
