{ pkgs, ... }:
let
  macport = pkgs.emacs-unstable.overrideAttrs (old: {
    configureFlags = old.configureFlags ++ [
      "--with-cairo"
      "--with-json"
    ];
    patches = (old.patches or [ ]) ++ [
      # Fix OS window role (needed for window managers like yabai)
      (pkgs.fetchpatch {
        url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
        sha256 = "+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
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

  emacs = if pkgs.stdenv.isLinux then pkgs.emacs-unstable else macport;
  banner = pkgs.stdenv.mkDerivation {
    name = "witchmacs-banner";
    src = pkgs.fetchurl {
      url = "https://snackon.github.io/images/witchmacs_icon.png";
      hash = "sha256-jZ1rpRkopYGBWcAs01pE1KknHNx/cCcP4sKWoa+8ad4=";
    };

    dontUnpack = true;

    installPhase = ''
      mkdir -p $out/
      convert $src -resize 25% $out/witchmacs_icon.png
    '';

    buildInputs = [ pkgs.imagemagick ];
  };
in
{
  home.file.".config/doom/banner.png".source = "${banner}/witchmacs_icon.png";

  programs.emacs = {
    enable = true;
    package = emacs;
    extraPackages =
      epkgs: with epkgs; [
        vterm
        all-the-icons
        nerd-icons
        # treesit-grammars.with-all-grammars
      ];
  };

  services.emacs = {
    enable = true;
    # defaultEditor = true;
    startWithUserSession = true;
    package = emacs;

    client.enable = true;
  };

  home = {
    sessionVariables = {
      JDTLS_PATH = "${pkgs.jdt-language-server}/share/java/jdtls";
    };

    packages = with pkgs; [
      ripgrep
      fd
      jq

      ispell
      languagetool
      nixd
      texlab
      sumneko-lua-language-server
      jdt-language-server
      stylua
      nodePackages_latest.prettier
      nodePackages_latest.typescript-language-server
      nodePackages_latest.eslint
      python311Packages.pytest
      python311Packages.pyflakes
      python311Packages.debugpy
      nodePackages_latest.svelte-language-server

      html-tidy
      shellcheck
      pyright
      cppcheck
      clang-tools
      nixfmt-rfc-style
      plantuml
      black

      shfmt

      # DAP protocols
      lldb
      (aspellWithDicts (
        ds: with ds; [
          en
          id
          en-computers
          en-science
        ]
      ))
      zstd
    ];
  };
  services.syncthing = {
    enable = true;
  };
}
