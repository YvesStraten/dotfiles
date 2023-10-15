{ pkgs, ... }: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages = epkgs:
      with epkgs; [
        vterm
        vterm-toggle
        magit
        all-the-icons
        tree-sitter-langs
        company
        company-box
        org-roam
        org-roam-ui
      ];
  };

  home.packages = with pkgs; [
    openjdk17
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

    # DAP protocols
    lldb
  ];

  services.emacs = {
    enable = true;
    defaultEditor = true;
    client = { enable = true; };
    startWithUserSession = true;
  };

  services.syncthing = { enable = true; };
}
