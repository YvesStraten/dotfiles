{ pkgs, ... }:
let
  pdfHTML = pkgs.stdenv.mkDerivation
    {
      name = "pdf2html";
      src = pkgs.fetchurl {
        url = "https://github.com/pdf2htmlEX/pdf2htmlEX/releases/download/v0.18.8.rc1/pdf2htmlEX-0.18.8.rc1-master-20200630-Ubuntu-bionic-x86_64.deb";
        sha256 = "1js27fb8pdvrj5azj2149f6hhb3qmkxw142n9rn29syhxmj6s044";
      };

      nativeBuildInputs = with pkgs; [
        autoPatchelfHook
        dpkg
      ];

      buildInputs = with pkgs; [
        glibc
        gcc-unwrapped
        libjpeg_original
        freetype
        libxml2
        glib
        zlib
        fontconfig
        libjpeg_original
        libjpeg
        cairo
      ];

      dontUnpack = true;

      installPhase = ''
        mkdir -p $out
        dpkg -x $src $out
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
      okular
      gnome.nautilus
      gnome.gnome-clocks
      gnome.pomodoro
      tootle
      gwenview
      gscan2pdf
      gimp
      filezilla
      krename
      htop

      # For notes
      nb
      nmap
      pandoc
      # pdfHTML

      ani-cli
    ];
}
