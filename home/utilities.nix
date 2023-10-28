{ pkgs, lib, ... }:
{
  home.packages = with pkgs;
    [
      tesseract
      brave
      tor-browser-bundle-bin
      libreoffice
      rclone
      rclone-browser
      imagemagick
      ghostscript_headless
      whatsapp-for-linux
      tootle
      gscan2pdf
      gimp
      filezilla
      krename
      htop

      pandoc

      nix-prefetch-scripts
      ani-cli
    ]
    ++ (with pkgs.gnome; [
      nautilus
      gnome-clocks
      pomodoro
      eog
      geary
      evince
      gnome-disk-utility
    ]);

  programs.thunderbird = {
    enable = true;
    profiles.yvess = {
      isDefault = true;
    };
  };
}
