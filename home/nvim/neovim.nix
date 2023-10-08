{ pkgs
, config
, ...
}: {
  programs.neovim = {
    enable = true;
    defaultEditor = true;
  };

  home.file.".config/nvim" = {
    recursive = true;
    source = pkgs.stdenv.mkDerivation {
      name = "nvchad";
      src = pkgs.fetchFromGitHub {
        owner = "Nvchad";
        repo = "Nvchad";
        rev = "13e9b0f458c3e5e95ac05a10822f26dbb1aa03cb";
        sha256 = "06k21p4w856ms405yg6qypj2sxh8jd4f606cr318qdpgrrb98n3y";
      };

      dontUnpack = true;

      installPhase = ''
        mkdir -p $out
        cp -R $src/* $out
      '';
    };
  };


  home.packages = with pkgs; [
    neovide
    python310Packages.pynvim
    zulu8
    languagetool

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
    # nixpkgs-fmt
    nixfmt

    # DAP protocols
    lldb
  ];

  services.syncthing = {
    enable = true;
  };
}
