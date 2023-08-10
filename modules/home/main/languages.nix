{pkgs, ...}: {
  home.packages = with pkgs; [
    python310
    pipx
    virtualenv

    # texlive.combined.scheme-full
  ];
}
