{ pkgs
, self
, ...
}: {
  home.packages =
    if pkgs.stdenv.isLinux
    then
      (with pkgs;
      [
        tesseract
        tor-browser-bundle-bin
        libreoffice
        rclone
        rclone-browser
        imagemagick

        gscan2pdf
        gimp
        filezilla
        krename
        btop

        pandoc

        nix-prefetch-scripts
        ani-cli-rofi

        qpwgraph
        soundwireserver
      ]) else
      (with pkgs; [
        tesseract
        imagemagick
        inkscape
        yazi
        spotify
        iina
        ani-cli
        xcbuild
        self.packages."aarch64-darwin".skim

        gimp
        btop

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
