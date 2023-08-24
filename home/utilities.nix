{ pkgs, ... }: {
  home.packages = with pkgs; [
    tesseract
    brave
    libreoffice
    rclone
    rclone-browser
    ani-cli
    okular
    gnome.nautilus
    gnome.gnome-clocks
    gnome.pomodoro
    tootle
    gwenview
    gscan2pdf
    gimp
    filezilla
    krename

    ffmpeg
    neofetch
  ];
}
