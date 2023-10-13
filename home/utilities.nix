{ pkgs, lib, ... }:
let
  pdfHTML = pkgs.stdenv.mkDerivation
    {
      name = "pdf2html";
      src = pkgs.fetchurl {
        url = "https://github.com/pdf2htmlEX/pdf2htmlEX/releases/download/v0.18.8.rc1/pdf2htmlEX-0.18.8.rc1-master-20200630-Ubuntu-bionic-x86_64.deb";
        sha256 = "1js27fb8pdvrj5azj2149f6hhb3qmkxw142n9rn29syhxmj6s044";
      };

      nativeBuildInputs = with pkgs; [
        dpkg
      ];

      buildInputs = with pkgs; [
        glibc
        gcc-unwrapped
        freetype
        libxml2
        glib
        zlib
        fontconfig
        libjpeg
        cairo
      ];

      dontUnpack = true;

      installPhase = ''
        mkdir -p $out
        dpkg -x $src $out
      '';
    };

  ani-cli-new = pkgs.stdenv.mkDerivation {
    name = "ani-cli";
    src = pkgs.fetchFromGitHub {
      owner = "pystardust";
      repo = "ani-cli";
      rev = "aef48eed709921403029f48d8b3c221a8cc8f8f2";
      sha256 = "06is3bxsqidr5p83gqsayvcw402jf0vbgpsjg43wyrjaiw3g2qc1";
    };

    nativeBuildInputs = with pkgs; [
      mpv
      gnugrep
      gnused
      curlWithGnuTls
      ffmpeg
      aria
      fzf
    ];

    dontUnpack = true;

    installPhase = ''
      mkdir -p $out/bin
      cp $src/ani-cli $out/bin
    '';
  };
in
{
  home.packages = with pkgs;
    [
      tesseract
      brave
      libreoffice
      rclone
      rclone-browser
      gnome.nautilus
      gnome.gnome-clocks
      gnome.pomodoro
      gnome.eog
      gnome.geary
      gnome.evince
      whatsapp-for-linux
      gnome-disks
      tootle
      gscan2pdf
      gimp
      filezilla
      krename
      htop

      # For notes
      nb
      nmap
      pandoc
      pdfHTML

      nix-prefetch-scripts

      ani-cli-new
    ];

  programs.thunderbird = {
    enable = true;
    profiles.yvess = {
      isDefault = true;
    };
  };
}
