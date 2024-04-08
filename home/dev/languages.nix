{ pkgs, ... }: {
  imports = [
    ../../config/jupyter/jupyter.nix
  ];

  services.jupyter-notebook.enable = true;

  home.packages = with pkgs; [
    (python311.withPackages (ps:
      with ps; [
        yt-dlp
        spotdl
        pygments
        tkinter
        jupyter
      ]))

    texlive.combined.scheme-full
    nodejs_21
    rustup
    yarn
    openjdk19

    sqlite

    unzip
    sshfs
    ffmpeg
    neofetch
    (hiPrio gcc)
    clang
  ];

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };
}
