{
  stdenv,
  writeShellScript,
  makeDesktopItem,
  fd,
  xdg-user-dirs,
  rofi,
  swww,
}:
stdenv.mkDerivation rec {
  name = "wall-switch";
  src = writeShellScript "wall" ''
    ${fd}/bin/fd . --type f $(${xdg-user-dirs}/bin/xdg-user-dir PICTURES)/Wallpapers | ${rofi}/bin/rofi -dmenu | xargs ${swww}/bin/swww img
  '';

  dontUnpack = true;

  desktop = makeDesktopItem {
    name = "Waller";
    desktopName = "Wallpaper";
    comment = "Program that swaps wallpapers";
    genericName = "Wallpaper";
    categories = ["Utility"];
    exec = "${src}";
  };

  installPhase = ''
    mkdir -p $out/bin
    mkdir -p $out/share/applications/
    cp $src $out/bin
    cp ${desktop}/share/applications/* $out/share/applications
  '';
}
