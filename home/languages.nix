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

    # JS
    nodejs_20
    yarn

    # C#
    dotnet-sdk
    mongosh
    mono

    unzip
    cargo
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
