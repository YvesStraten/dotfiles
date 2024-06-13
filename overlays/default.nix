{
  inputs,
  pkgs,
  lib,
  ...
}:
{
  nixpkgs.overlays = [
    inputs.nur.overlay
    inputs.yazi.overlays.default
    inputs.firefox-darwin.overlay
    inputs.emacs-overlay.overlay

    (final: prev: {
      yvess = (prev.yvess or { }) // (import ../packages/default.nix { inherit (prev) pkgs; });

      jdt-language-server-wsl =
        assert (lib.assertMsg (prev.jdt-language-server.version == "1.31.0") "Wsl? is this still needed?");
        pkgs.callPackage ../packages/jdt.nix { };

      picom-pijulius =
        assert (lib.assertMsg (prev.picom-pijulius != null) "Now in nixpkgs");
        pkgs.callPackage ../packages/picom-pijulius.nix { };

      sddm = prev.sddm.overrideAttrs (oldAttrs: {
        buildInputs = oldAttrs.buildInputs ++ [
          final.qt5.qtquickcontrols2
          final.qt5.qtgraphicaleffects
        ];
      });

      libsForQt5 = prev.libsForQt5 // {
        sddm = prev.libsForQt5.sddm.overrideAttrs (oldAttrs: {
          buildInputs = oldAttrs.buildInputs ++ [ final.qt5.qtgraphicaleffects ];
        });
      };

      ani-cli-rofi = prev.ani-cli.overrideAttrs (
        oldAttrs:
        let
          desktop = pkgs.makeDesktopItem {
            name = "ani-cli";
            desktopName = "Anime cli";
            comment = "A cli program to watch anime";
            genericName = "Anime player";
            categories = [ "Video" ];
            exec = "ani-cli --rofi";
          };
        in
        {
          installPhase = ''
            mkdir -p $out/share/applications
            cp ${desktop}/share/applications/* $out/share/applications
            ${oldAttrs.installPhase}
          '';
        }
      );
    })
  ];
}
