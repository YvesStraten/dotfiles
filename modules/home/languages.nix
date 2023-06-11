{pkgs, ...}: {
  home.packages = with pkgs; [
    nodejs
    yarn
    gcc
    python310
    quarto
    jupyter
    texlive.combined.scheme-medium

    texlab
    sumneko-lua-language-server
    stylua
    nodePackages_latest.prettier
    shellcheck
    nodePackages_latest.pyright
  ];
}
