{pkgs, ...}: {
  imports = [
    ../../config/jupyter/jupyter.nix
  ];

  services.jupyter-notebook.enable = true;

  home.sessionVariables = {
    DOTNET_ROOT = "${pkgs.dotnet-sdk}";
  };

  home.packages = with pkgs; [
    lazygit
    (python311.withPackages (ps:
      with ps; [
        yt-dlp
        spotdl
        pygments
        tkinter
        jupyter
        pillow
      ]))

    texlive.combined.scheme-full
    nodejs
    rustup
    yarn
    openjdk
    dotnet-sdk

    unzip
    sshfs
    ffmpeg
    neofetch
    (hiPrio gcc)
    clang
    gnumake
    drawio
  ];

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };
}
