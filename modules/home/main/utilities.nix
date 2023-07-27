{pkgs, ...}: {
  home.packages = with pkgs; [
    tesseract
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
    neofetch
  ];
}
