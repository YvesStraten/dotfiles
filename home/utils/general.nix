{pkgs, ...}: {
  home.packages =
    if pkgs.stdenv.isLinux
    then
      (with pkgs; [
        tesseract
        rsync
        libreoffice
        rclone
        spotify
        obs-studio

        gimp
        filezilla
        btop
        vesktop
        nautilus
        teams-for-linux

        pandoc

        nix-prefetch-scripts
        ani-cli-rofi

        qpwgraph
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
