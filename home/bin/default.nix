{pkgs, ...}: let
  encode = pkgs.writeShellScriptBin "encode-mp4" ''
    set -e
    origdir="./original"
    shopt -s extglob nullglob

    if [ ! -d "$origdir" ];
    then
      echo "Creating $origdir directory."
      mkdir "$origdir"
    fi

    for vid in *.mp4; do
      noext="''${vid%.mp4}"
      ${pkgs.ffmpeg}/bin/ffmpeg -i "$vid" -acodec pcm_s16le -vcodec copy "''${noext// /_}.mov"
        mv "$vid" "$origdir"
    done
  '';

  playlist = pkgs.writeShellScriptBin "playlist" ''
    ${builtins.readFile ./playlist.sh}
  '';
in {
  home.packages = [
    encode
    playlist
  ];
}
