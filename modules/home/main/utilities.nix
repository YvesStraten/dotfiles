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
    tootle
    gwenview
    gscan2pdf
    gimp
    filezilla
    krename

    mpv
    ffmpeg
    neofetch
  ];

  services.udiskie = {
    enable = true;
    automount = true;
    tray = "never";
  };
}
