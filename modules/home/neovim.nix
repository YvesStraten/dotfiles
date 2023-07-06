{pkgs, ...}: {
  home.packages = with pkgs; [
    neovim
    neovide
    tmux
    tmuxinator
    python310Packages.pynvim
    ripgrep
    fd
    fzf
    zathura
    gh
    git
    neofetch

    # LSP servers
    /* texlab
    omnisharp-roslyn
    sumneko-lua-language-server
    stylua
    nodePackages_latest.prettier
    nodePackages_latest.vscode-html-languageserver-bin
    html-tidy
    shellcheck
    nodePackages_latest.pyright
    cppcheck */
    alejandra
    /* clang-tools
    
    # DAP protocols
    lldb */
  ];
}
