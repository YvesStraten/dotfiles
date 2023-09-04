{pkgs, ...}: {
  home.packages = with pkgs; [
    tesseract
    brave
    libreoffice
    rclone
    rclone-browser
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
    htop

    ani-cli
  ];
}
