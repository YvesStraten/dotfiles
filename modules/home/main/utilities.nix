{pkgs, ...}: {
  home.packages = with pkgs; [
    tesseract
    brave
    libreoffice
    rclone
    rclone-browser
    ranger
    ani-cli
    okular
    gnome.nautilus
    gnome.gnome-clocks
    gnome.pomodoro
    gwenview
    gscan2pdf
    gimp
    filezilla

    mpv
    ffmpeg
    neofetch
  ];
}
