{ pkgs
, fetchpatch
, ...
}: {
  programs.emacs = {
    enable = true;
    package =
      if pkgs.stdenv.isLinux
      then pkgs.emacs-pgtk
      else pkgs.emacs29-macport;
    extraPackages = epkgs:
      with epkgs; [
        vterm
        treesit-grammars.with-all-grammars
        all-the-icons
        pdf-tools
      ];
  };

  home.packages = with pkgs; [
    ripgrep
    fd

    ispell
    languagetool
    rnix-lsp
    jdt-language-server
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
    plantuml

    # DAP protocols
    lldb
  ];

  services.syncthing = { enable = true; };
}
