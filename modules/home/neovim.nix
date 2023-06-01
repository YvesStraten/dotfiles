{pkgs, ...}: {
  home.packages = with pkgs; [
    neovim
    tmux
    tmuxinator
    python310Packages.pynvim
    ripgrep
    fd
    fzf
  ];
}
