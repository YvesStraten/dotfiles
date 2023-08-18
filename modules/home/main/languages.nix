{pkgs, ...}: {
  home.packages = with pkgs; [
    python310
    python310Packages.matplotlib
    pipx
    virtualenv

    texlive.combined.scheme-full
  ];
}
