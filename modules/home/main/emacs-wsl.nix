{pkgs, ...}: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacsPgtk;
    extraPackages = epkgs: with epkgs; [
      vterm
    ];
  };

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  home.packages = with pkgs; [
    zulu8
    languagetool
  ];

  services.emacs = {
    enable = true;
    defaultEditor = true;
    client = {
      enable = true;
      arguments = [
        "-c"
      ];
    };
    socketActivation.enable = true;
  };

  services.syncthing = {
    enable = true;
    tray.enable = true;
  };
}
