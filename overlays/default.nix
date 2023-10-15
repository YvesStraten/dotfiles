{ pkgs, config }: {
  nixpkgs.overlays = [
    (
      final: prev: {
        ani-cli = prev.ani-cli.overrideAttrs
          (o: {

            propagatedBuildInputs with pkgs; =
              [
                mpv
                gnugrep
                gnused
                curlWithGnuTls
                ffmpeg
                aria
                fzf
              ];

            desktopItem = prev.makeDesktopItem
              {
                name = "ani-cli";
                desktopName = "Anime cli";
                comment = "A cli program to watch anime";
                genericName = "cli program";
                categories = [ "Video" ];
                exec = "ani-cli --rofi";
              };
          });
      };
    );
  ];
}
