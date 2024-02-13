{
  pkgs,
  ...
}: {
  home.packages =
    if pkgs.stdenv.isLinux
    then
      (with pkgs;
        [
          tesseract
          brave
          tor-browser-bundle-bin
          libreoffice
          rclone
          rclone-browser
          imagemagick
          whatsapp-for-linux

          gscan2pdf
          gimp
          filezilla
          krename
          htop

          pandoc

          nix-prefetch-scripts
          ani-cli-rofi

          qpwgraph
          soundwireserver
        ]
        ++ (with pkgs.gnome; [
          nautilus
          gnome-clocks
          pomodoro
          eog
          geary
          evince
          gnome-disk-utility
        ]))
    else
      (with pkgs; [
        tesseract
        imagemagick
        inkscape
        yazi
        spotify
        discord
        iina
        ani-cli
        xcbuild
        yvess.skim

        gimp
        htop

        pandoc

        nix-prefetch-scripts
      ]);

  services.syncthing = {
    enable = true;
  };
  #  programs.thunderbird = {
  #    enable = true;
  #    profiles.yvess = {
  #      isDefault = true;
  #    };
  #  };
}
