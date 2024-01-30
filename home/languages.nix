{pkgs, ...}: {
  home.packages = with pkgs; [
    (python311.withPackages (ps:
      with ps; [
        yt-dlp
        spotdl
        pygments
      ]))

    texlive.combined.scheme-full
    nodejs_21
    rustup
    yarn
    openjdk19

    unzip
    sshfs
    ffmpeg
    neofetch
    clang
  ];

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };
}
