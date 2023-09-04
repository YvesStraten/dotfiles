{pkgs, ...}: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages = epkgs:
      with epkgs; [
        vterm
        vterm-toggle
        magit
        all-the-icons
        tree-sitter-langs
      ];
  };

  home.file.".emacs.d/init.el" = {
    source = ./init.el;
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
