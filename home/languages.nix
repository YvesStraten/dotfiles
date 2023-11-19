{ pkgs, ... }: {
  home.packages = with pkgs; [
    (python311.withPackages (ps:
      with ps; [
        yt-dlp
        spotdl
        pygments
      ]))

    (pkgs.texlive.combine {
      inherit
        (pkgs.texlive)
        scheme-full
        ;
    })

    ripgrep
    fd
    fzf

    # Cpp
    gcc
    cmake

    # C#
    dotnet-sdk
    mono

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
