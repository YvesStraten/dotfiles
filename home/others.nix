{ config
, pkgs
, ...
}: {
  home.file = {
    ".config/i3" = {
      source = ./i3;
      recursive = true;
    };

    ".config/polybar/" = {
      source = ./polybar/bitmap;
      recursive = true;
    };

    ".local/share/fonts" = {
      source = ./fonts;
      recursive = true;
    };
  };
}
