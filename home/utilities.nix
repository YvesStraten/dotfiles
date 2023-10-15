{ pkgs, lib, ... }:
{
  home.packages = with pkgs;
    [
      tesseract
      brave
      libreoffice
      rclone
      rclone-browser
      gnome.nautilus
      gnome.gnome-clocks
      gnome.pomodoro
      gnome.eog
      gnome.geary
      gnome.evince
      gnome.gnome-disk-utility
      whatsapp-for-linux
      tootle
      gscan2pdf
      gimp
      filezilla
      krename
      htop

      # For notes
      nb
      nmap
      pandoc

      nix-prefetch-scripts
      ani-cli
    ];

  programs.thunderbird = {
    enable = true;
    profiles.yvess = {
      isDefault = true;
    };
  };
}
