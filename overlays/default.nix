{ inputs, pkgs, ... }: {
  nixpkgs.overlays = [
    (
      final: prev: {
        sddm = prev.sddm.overrideAttrs
          (o: {
            buildInputs = o.buildInputs ++ [
              final.qt5.qtquickcontrols2
              final.qt5.qtgraphicaleffects
            ];
          });

        libsForQt5 = prev.libsForQt5 // {
          sddm = prev.libsForQt5.sddm.overrideAttrs
            (o: {
              buildInputs = o.buildInputs ++ [ final.qt5.qtgraphicaleffects ];
            });
        };

        ani-cli =
          let
            desktop = pkgs.makeDesktopItem
              {
                name = "ani-cli";
                desktopName = "Anime cli";
                comment = "A cli program to watch anime";
                genericName = "Anime player";
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
