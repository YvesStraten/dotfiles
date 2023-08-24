{ pkgs, ... }: {
  programs.neovim.enable = true;

  home.packages = with pkgs; [
    neovide
    python310Packages.pynvim
    ripgrep
    fd
    fzf
    gcc
    nodejs_20
    unzip
    cargo
    neofetch

    # LSP servers
    texlab
    omnisharp-roslyn
    sumneko-lua-language-server
    stylua
    nodePackages_latest.prettier
    nodePackages_latest.vscode-html-languageserver-bin
    nodePackages_latest.typescript-language-server
    html-tidy
    rnix-lsp
    shellcheck
    nodePackages_latest.pyright
    cppcheck
    alejandra
    clang-tools
    nixpkgs-fmt

    # DAP protocols
    lldb
  ];

  home.file.".config/nvim/lua/custom" = {
    source = ./custom;
    recursive = true;
  };
}
