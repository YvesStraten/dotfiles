{ pkgs, fetchpatch, ... }:
let
  emacs-mac = pkgs.emacs-pgtk.overrideAttrs (o: {
    patches = (o.patches or [ ]) ++ [
      # Fix OS window role (needed for window managers like yabai)
      (fetchpatch {
        url =
          "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
        sha256 = "0c41rgpi19vr9ai740g09lka3nkjk48ppqyqdnncjrkfgvm2710z";
      })
      # Use poll instead of select to get file descriptors

      (fetchpatch {
        url =
          "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/poll.patch";
        sha256 = "0j26n6yma4n5wh4klikza6bjnzrmz6zihgcsdx36pn3vbfnaqbh5";
      })
      # Enable rounded window with no decoration
      (fetchpatch {
        url =
          "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/round-undecorated-frame.patch";
        sha256 = "111i0r3ahs0f52z15aaa3chlq7ardqnzpwp8r57kfsmnmg6c2nhf";
      })
      # Make Emacs aware of OS-level light/dark mode
      (fetchpatch {
        url =
          "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/system-appearance.patch";
        sha256 = "14ndp2fqqc95s70fwhpxq58y8qqj4gzvvffp77snm2xk76c1bvnn";
      })
    ];
  });
in
{
  programs.emacs = {
    enable = true;
    package = if pkgs.stdenv.isLinux then pkgs.emacs-pgtk else emacs-mac;
    extraPackages = epkgs:
      with epkgs; [
        vterm
        treesit-grammars.with-all-grammars
        vterm-toggle
        magit
        all-the-icons
        org-roam
        org-roam-ui
      ];
  };

  home.file.".emacs.d/marivector.png" = { source = ./marivector.png; };
  home.file.".emacs.d/init.el" = { source = ./init.el; };
  home.file.".emacs.d/early-init.el" = { source = ./early-init.el; };

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

  services.emacs = {
    enable = true;
    defaultEditor = true;
  };

  services.syncthing = { enable = true; };
}
