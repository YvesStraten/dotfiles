{ pkgs, ... }: {
  home.packages = if pkgs.stdenv.isLinux then
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
      # ani-cli
      xcbuild

      gimp
      btop

      pandoc

      nix-prefetch-scripts
    ]);

  services.syncthing = { enable = true; };
  #  programs.thunderbird = {
  #    enable = true;
  #    profiles.yvess = {
  #      isDefault = true;
  #    };
  #  };
}
