{
  config,
  pkgs,
  ...
}: {
  home.file = {
    ".config/dunst" = {
      source = ../dots/dunst;
      recursive = true;
    };

    ".config/hypr" = {
      source = ../dots/hypr;
      recursive = true;
    };

    ".config/i3" = {
      source = ../dots/i3;
      recursive = true;
    };

    ".config/kitty" = {
      source = ../dots/kitty;
      recursive = true;
    };

    ".config/mpv" = {
      source = ../dots/mpv;
      recursive = true;
    };

    ".config/nvim/lua/custom" = {
      source = ../dots/nvim/custom;
      recursive = true;
    };

    ".config/picom" = {
      source = ../dots/picom;
      recursive = true;
    };

    ".config/qtile" = {
      source = ../dots/qtile;
      recursive = true;
    };

    ".config/ranger" = {
      source = ../dots/ranger;
      recursive = true;
    };

    ".config/rofi" = {
      source = ../dots/rofi;
      recursive = true;
    };

    ".config/swaylock" = {
      source = ../dots/swaylock;
      recursive = true;
    };

    ".tmux.conf" = {
      source = ../dots/tmux/tmux.conf;
    };

    ".config/waybar" = {
      source = ../dots/waybar;
      recursive = true;
    };

    ".config/wlogout" = {
      source = ../dots/wlogout;
      recursive = true;
    };

    ".config/zathura" = {
      source = ../dots/zathura;
      recursive = true;
    };

    ".config/polybar/" = {
      source = ../dots/polybar/bitmap;
      recursive = true;
    };

    ".local/bin/" = {
      source = ../dots/scripts;
      recursive = true;
    };

    ".local/share/fonts" = {
      source = ../dots/polybar/fonts;
      recursive = true;
    };
  };
}
