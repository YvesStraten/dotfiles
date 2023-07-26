{pkgs, ...}: {
  home.packages = with pkgs; [
    python310
    pipx

    texlive.combined.scheme-medium

    virtualenv
  ];
}
