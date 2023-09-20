{pkgs, ...}: {
  home.packages = with pkgs; [
    (python311.withPackages (ps:
      with ps; [
        matplotlib
        requests
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
    conan
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
    aria
    neofetch
    sqlite
  ];
}
