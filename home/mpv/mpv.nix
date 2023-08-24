{ pkgs, ... }: {
  home.file.".config/mpv/mpv.conf" = {
    source = ./mpv.conf;
  };
}
