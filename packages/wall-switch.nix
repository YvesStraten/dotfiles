{
  stdenvNoCC,
  writeShellScript,
  makeDesktopItem,
  lib,
  fd,
  xdg-user-dirs,
  rofi,
  swww,
}:
stdenvNoCC.mkDerivation rec {
  name = "wall-switch";
  src = writeShellScript "wall" ''
        Main="${fd}/bin/fd . --type f $(${xdg-user-dirs}/bin/xdg-user-dir PICTURES)/Wallpapers | ${rofi}/bin/rofi -dmenu"
    val=$(eval $Main)

        ${swww}/bin/swww img $val

    printf "#!/usr/bin/env bash\nswww init\nswww img $val" > ~/.config/hypr/scripts/wall.sh | chmod +x ~/.config/hypr/scripts/wall.sh
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

  meta = with lib; {
    platforms = with platforms; linux;
  };
}
