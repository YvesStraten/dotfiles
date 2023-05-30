{pkgs, ...}: {
  home.packages = with pkgs; [
    neovim
    tmux
    tmuxinator
    neovim-qt
    python310Packages.pynvim
    ripgrep
    fd
    fzf
  ];
}
