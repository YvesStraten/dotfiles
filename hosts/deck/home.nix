{
  pkgs,
  lib,
  ...
}: {
  custom = {
    fish.enable = true;
    tmux.enable = true;
    kitty.enable = true;
    firefox.enable = true;
    utils.enable = lib.mkForce false;
    emacs.enable = true;

    general = {
      enable = true;
      extraPackages = with pkgs; [
        libreoffice-qt6-fresh
        zotero
        temurin-jre-bin-17
        teams-for-linux
      ];
    };

    theming = {
      enable = true;
      gtk.enable = false;
      qt.enable = false;
    };
  };

  services.emacs.enable = lib.mkForce false;

  home.packages = [
    pkgs.texlive.combined.scheme-medium
    pkgs.wl-clipboard
  ];

  programs.direnv = {
    enable = true;
  };
}
