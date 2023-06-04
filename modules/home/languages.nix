{pkgs, ...}: {
  home.packages = with pkgs; [
    nodejs
    yarn
    gcc
    omnisharp-roslyn
    python39
    python39Packages.pillow
    python39Packages.pip
    virtualenv
    quarto
    jupyter
    python39Packages.numpy
    texlive.combined.scheme-medium

    texlab
    sumneko-lua-language-server 
    stylua
    nodePackages_latest.prettier 
    shellcheck

  ];
}
