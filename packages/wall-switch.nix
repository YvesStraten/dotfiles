{
  stdenvNoCC,
  writeShellApplication,
  makeDesktopItem,
  lib,
  xdg-user-dirs,
  rofi,
  imagemagick,
}:
let
  script = writeShellApplication {
    name = "wall-script";
    text = builtins.readFile ./wallpaper.sh;

    runtimeInputs = [
      xdg-user-dirs
      rofi
      imagemagick
    ];
  };

  desktop = makeDesktopItem {
    name = "Waller";
    desktopName = "Wallpaper";
    comment = "Program that swaps wallpapers";
    genericName = "Wallpaper";
    categories = [ "Utility" ];
    exec = "${lib.getExe script}";
  };
in
stdenvNoCC.mkDerivation {
  name = "wall-switch";

  dontUnpack = true;
  installPhase = ''
    mkdir -p $out/bin
    mkdir -p $out/share/applications/
    cp ${lib.getExe script} $out/bin
    cp ${desktop}/share/applications/* $out/share/applications
  '';

  meta = with lib; {
    platforms = with platforms; linux;
  };
}
