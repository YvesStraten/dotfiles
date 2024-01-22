{
  pkgs,
  fetchpatch,
  ...
}: {
  programs.emacs = {
    enable = true;
    package =
      if pkgs.stdenv.isLinux
      then pkgs.emacs-pgtk
      else pkgs.emacs-macport;
    extraPackages = epkgs:
      with epkgs; [
        vterm
        treesit-grammars.with-all-grammars
        auctex-latexmk
        vterm-toggle
        magit
        all-the-icons
        org-roam
        org-roam-ui
      ];
  };

  home.file.".emacs.d/marivector.png" = {source = ./marivector.png;};
  home.file.".emacs.d/init.el" = {source = ./init.el;};

  home.packages = with pkgs; [
    openjdk17
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

  services.syncthing = {enable = true;};
}
