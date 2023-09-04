{pkgs, config, ...}:{
  programs.neovim.enable = true;
  home.packages = with pkgs; [
    neovide
    python310Packages.pynvim
  ];

  services.syncthing = {
    enable = true;
  };
}
