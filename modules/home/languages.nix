{pkgs, ...}: {
  home.packages = with pkgs; [
    nodejs
    yarn
    gcc
    python310
    quarto
    jupyter
    texlive.combined.scheme-medium

    virtualenv
  ];
}
