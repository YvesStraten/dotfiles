{ pkgs, ... }: {
  home.packages = with pkgs; [
    (python311.withPackages (ps:
      with ps; [
        yt-dlp
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
    mongosh
    mono

    unzip
    sshfs
    ffmpeg
    neofetch
    sqlite
  ];

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };
}
