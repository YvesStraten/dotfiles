{pkgs, ...}: {
  home.packages = with pkgs; [
    neovim
    tmux
    tmuxinator
    python310Packages.pynvim
    ripgrep
    fd
    fzf

    # LSP servers
    texlab
    omnisharp-roslyn
    sumneko-lua-language-server
    stylua
    nodePackages_latest.prettier
    shellcheck
    nodePackages_latest.pyright
  ];
}
