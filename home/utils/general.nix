{pkgs, ...}: {
  home.packages =
    if pkgs.stdenv.isLinux
    then
      (with pkgs; [
        tesseract
        rsync
        libreoffice
        rclone
        imagemagick
        spotify
        obs-studio

        gimp
        filezilla
        btop

        pandoc

        nix-prefetch-scripts
        ani-cli-rofi
        unityhub

        qpwgraph
        soundwireserver
      ])
    else
      (with pkgs; [
        tesseract
        rsync
        imagemagick
        inkscape
        yazi
        spotify
        iina
        rclone
        skimpdf
        xcbuild

        gimp
        btop

        pandoc

        nix-prefetch-scripts
      ]);
}
