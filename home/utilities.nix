{ pkgs, ... }:
let
  pdfHTML = pkgs.stdenv.mkDerivation
    {
      name = "pdf2html";
      src = pkgs.fetchurl {
        url = "https://github.com/pdf2htmlEX/pdf2htmlEX/releases/download/v0.18.8.rc1/pdf2htmlEX-0.18.8.rc1-master-20200630-alpine-3.12.0-x86_64.tar.gz";
        sha256 = "0mspqx683dbyggs2f3zi44sn7mjyqlhd8y692fm2iwv9rwvj2xwz";
      };

      installPhase = ''
        mkdir -p $out/
        cp -R * $out/
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
      pdfHTML

      ani-cli
    ];
}
