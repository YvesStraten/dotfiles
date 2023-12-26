{pkgs, ...}: {
  home.packages = with pkgs; [
    (python311.withPackages (ps:
      with ps; [
        yt-dlp
        spotdl
        pygments
      ]))

    texlive.combined.scheme-full

    unzip
    sshfs
    ffmpeg
    neofetch
  ];

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };
}
