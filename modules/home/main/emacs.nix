{pkgs, ...}: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
    extraPackages = epkgs: with epkgs; [
      vterm
      vterm-toggle
      magit
      all-the-icons
    ];
  };

    home.file.".emacs.d" = {
      source = ../dots/emacs;
      recursive = true;
    };

  home.packages = with pkgs; [
    zulu8
    languagetool
  ];

  services.emacs = {
    enable = true;
    defaultEditor = true;
    client = {
      enable = true;
    };
    startWithUserSession = true;
  };

  services.syncthing = {
    enable = true;
  };
}
