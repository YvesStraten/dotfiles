{ pkgs, ... }:
let
  emacs = if pkgs.stdenv.isLinux then pkgs.emacs-unstable else pkgs.emacs30;
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
      nil
      texlab
      sumneko-lua-language-server
      jdt-language-server
      stylua
      nodePackages_latest.prettier
      nodePackages_latest.typescript-language-server
      nodePackages_latest.eslint
      nodePackages_latest.svelte-language-server

      html-tidy
      shellcheck
      pyright
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
