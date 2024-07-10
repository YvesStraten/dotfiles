{ pkgs, ... }: {
  imports = [
    ../../config/jupyter/jupyter.nix
  ];

  services.jupyter-notebook.enable = true;

  home.sessionVariables = {
    DOTNET_ROOT = "${pkgs.dotnet-sdk}";
 };

  home.packages = with pkgs; [
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
    openjdk19
    dotnet-sdk

    unzip
    sshfs
    ffmpeg
    neofetch
    (hiPrio gcc)
    clang
    gnumake
  ];

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };
}
