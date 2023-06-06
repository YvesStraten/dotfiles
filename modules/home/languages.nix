{pkgs, ...}: {
  home.packages = with pkgs; [
    nodejs
    yarn
    gcc
    omnisharp-roslyn
    python310
    python310Packages.pillow
    python310Packages.pip
    virtualenv
    quarto
    jupyter
    python310Packages.numpy
    texlive.combined.scheme-medium
    pipenv

    texlab
    sumneko-lua-language-server 
    stylua
    nodePackages_latest.prettier 
    shellcheck
    nodePackages_latest.pyright
    statix

  ];
}
