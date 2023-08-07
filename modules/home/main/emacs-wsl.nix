{pkgs, ...}: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs: with epkgs; [
      vterm
    ];
  };

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
