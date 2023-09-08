{pkgs, config, ...}:{
  programs.neovim = {
    enable = true;
    defaultEditor = true;
  };

  home.packages = with pkgs; [
    neovide
    python310Packages.pynvim

    # LSP servers
    texlab
    omnisharp-roslyn
    sumneko-lua-language-server
    stylua
    nodePackages_latest.prettier
    nodePackages_latest.vscode-html-languageserver-bin
    nodePackages_latest.typescript-language-server
    nodePackages_latest.eslint
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

  services.syncthing = {
    enable = true;
  };
}