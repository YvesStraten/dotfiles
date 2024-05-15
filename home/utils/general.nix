{ pkgs, ... }: {
  home.packages = if pkgs.stdenv.isLinux then
    (with pkgs; [
      tesseract
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

      qpwgraph
      soundwireserver
    ])
  else
    (with pkgs; [
      tesseract
      imagemagick
      inkscape
      yazi
      spotify
      iina
      rclone
      skimpdf
      ani-cli
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
