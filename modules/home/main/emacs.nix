{pkgs, ...}: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  services.emacs = {
    enable = true;
    defaultEditor = true;
    client = {
      enable = true;
    };
  };
}
