{ inputs, pkgs, lib, ... }: {
  nixpkgs.overlays = [
    inputs.nur.overlay
    inputs.yazi.overlays.default
    inputs.firefox-darwin.overlay
    inputs.emacs-overlay.overlay

    (final: prev: {
      yvess = (prev.yvess or { })
        // (import ../packages/default.nix { inherit (prev) pkgs; });

      jdt-language-server-wsl =
        assert (lib.assertMsg (prev.jdt-language-server.version == "1.31.0" )
          "Wsl? is this still needed?");
        pkgs.callPackage ../packages/jdt.nix { };

      sddm = prev.sddm.overrideAttrs (o: {
        buildInputs = o.buildInputs
          ++ [ final.qt5.qtquickcontrols2 final.qt5.qtgraphicaleffects ];
      });

      libsForQt5 = prev.libsForQt5 // {
        sddm = prev.libsForQt5.sddm.overrideAttrs (o: {
          buildInputs = o.buildInputs ++ [ final.qt5.qtgraphicaleffects ];
        });
      };

      ani-cli-rofi = prev.ani-cli.overrideAttrs (o: rec {
        desktop = pkgs.makeDesktopItem {
          name = "ani-cli";
          desktopName = "Anime cli";
          comment = "A cli program to watch anime";
          genericName = "Anime player";
          categories = [ "Video" ];
          exec = "ani-cli --rofi";
        };

        installPhase = ''
          mkdir -p $out/share/applications
          cp ${desktop}/share/applications/* $out/share/applications
          ${o.installPhase}
        '';
      });
    })
  ];
}
