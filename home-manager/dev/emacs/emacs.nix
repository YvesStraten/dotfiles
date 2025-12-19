{
  config,
  options,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.custom.emacs;
  inherit (lib)
    mkIf
    mkEnableOption
    mkOption
    types
    ;
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
  options.custom = {
    emacs = {
      enable = mkEnableOption "Enable emacs";
      package = mkOption {
        default =
          with pkgs;
          ((emacsPackagesFor pkgs.emacs30-pgtk).emacsWithPackages (
            epkgs: with epkgs; [
              vterm
              all-the-icons
              nerd-icons
              treesit-grammars.with-all-grammars
            ]
          ));
        type = types.package;
        description = ''
          Emacs package to use
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    home.file.".config/emacs/banner.png".source = "${banner}/witchmacs_icon.png";

    programs.emacs = {
      enable = true;
      package = cfg.package;
    };

    services.emacs = {
      enable = true;
      package = cfg.package;
      client.enable = true;
      defaultEditor = true;
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
        zstd
      ];
    };
  };
}
