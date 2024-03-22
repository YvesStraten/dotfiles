{ pkgs
, ...
}:
let
  macport = pkgs.emacs-unstable.overrideAttrs (old: {
    configureFlags = old.configureFlags ++ [
      "--with-cairo"
      "--with-json"
    ];
    patches =
      (old.patches or [ ])
      ++ [
        # Fix OS window role (needed for window managers like yabai)
        (pkgs.fetchpatch {
          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
          sha256 = "+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
        })
        # Use poll instead of select to get file descriptors
        (pkgs.fetchpatch {
          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/poll.patch";
          sha256 = "jN9MlD8/ZrnLuP2/HUXXEVVd6A+aRZNYFdZF8ReJGfY=";
        })
        # Enable rounded window with no decoration
        (pkgs.fetchpatch {
          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/round-undecorated-frame.patch";
          sha256 = "uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
        })
        # Make Emacs aware of OS-level light/dark mode
        (pkgs.fetchpatch {
          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/system-appearance.patch";
          sha256 = "oM6fXdXCWVcBnNrzXmF0ZMdp8j0pzkLE66WteeCutv8=";
        })
      ];
  });

  emacs =
    if pkgs.stdenv.isLinux
    then pkgs.emacs-pgtk
    else macport;
in
{
  programs.emacs = {
    enable = true;
    package =
      emacs;
    extraPackages = epkgs:
      with epkgs; [
        vterm
        all-the-icons
        nerd-icons
        # treesit-grammars.with-all-grammars
        pdf-tools
      ];
  };

  home.packages = with pkgs; [
    ripgrep
    fd

    ispell
    languagetool
    nixd
    texlab
    sumneko-lua-language-server
    stylua
    nodePackages_latest.prettier
    nodePackages_latest.vscode-html-languageserver-bin
    nodePackages_latest.typescript-language-server
    nodePackages_latest.eslint
    html-tidy
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

  services.syncthing = {
    enable = true;
  };
}
