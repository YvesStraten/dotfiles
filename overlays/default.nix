{ inputs, pkgs, ... }: {
  nixpkgs.overlays = [
    (
      final: prev: {
        ani-cli =
          let
            desktop = pkgs.makeDesktopItem
              {
                name = "ani-cli";
                desktopName = "Anime cli";
                comment = "A cli program to watch anime";
                genericName = "cli program";
                categories = [ "Video" ];
                exec = "ani-cli --rofi";
              };
          in
          prev.ani-cli.overrideAttrs
            (o: {
              installPhase = ''
                mkdir -p $out/share/applications
                cp ${desktop}/share/applications/* $out/share/applications
                ${o.installPhase}
              '';
            });
      }
    )
  ];
}
