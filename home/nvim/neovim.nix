{ pkgs, ... }: {
  programs.neovim.enable = true;

  home.packages = with pkgs; [
    neovide
    python310Packages.pynvim
  ];

  home.file.".config/nvim/lua/custom" = {
    source = ./custom;
    recursive = true;
  };
}
