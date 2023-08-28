{ pkgs, ... }:
{
  home.packages = with pkgs; [
    (python311.withPackages (ps: with ps; [
      matplotlib
      requests
    ]))

    texlive.combined.scheme-full

    ripgrep
    fd
    fzf
    gcc
    nodejs_20
    unzip
    cargo

    # LSP servers
    texlab
    omnisharp-roslyn
    sumneko-lua-language-server
    stylua
    nodePackages_latest.prettier
    nodePackages_latest.vscode-html-languageserver-bin
    nodePackages_latest.typescript-language-server
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
}
