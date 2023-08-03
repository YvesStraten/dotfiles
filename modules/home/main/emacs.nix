{pkgs, ...}: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
  };

  services.emacs = {
    enable = true;
    defaultEditor = true;
    client = {
      enable = true;
    };
    socketActivation.enable = true;
  };
}
