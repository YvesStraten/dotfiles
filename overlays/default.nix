{
  inputs,
  pkgs,
  ...
}: {
  nixpkgs.overlays = [
    inputs.nur.overlay
    inputs.firefox-darwin.overlay

    (import (builtins.fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
      sha256 = "1fmwbnzdkxpf5631syqqcx4wpi5qd8406gvdqzb7pkc40ll5yskx";
    }))

    (final: prev: {
      yvess =
        (prev.yvess or {})
        // (import ../packages/default.nix {inherit (prev) pkgs;});

      nwg-displays = prev.nwg-displays.override {hyprlandSupport = true;};

      sddm = prev.sddm.overrideAttrs (o: {
        buildInputs =
          o.buildInputs
          ++ [final.qt5.qtquickcontrols2 final.qt5.qtgraphicaleffects];
      });

      libsForQt5 =
        prev.libsForQt5
        // {
          sddm = prev.libsForQt5.sddm.overrideAttrs (o: {
            buildInputs = o.buildInputs ++ [final.qt5.qtgraphicaleffects];
          });
        };

      ani-cli-rofi = prev.ani-cli.overrideAttrs (o: rec {
        desktop = pkgs.makeDesktopItem {
          name = "ani-cli";
          desktopName = "Anime cli";
          comment = "A cli program to watch anime";
          genericName = "Anime player";
          categories = ["Video"];
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
