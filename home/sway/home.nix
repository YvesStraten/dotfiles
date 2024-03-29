{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ../swaylock/swaylock.nix
    ../wlogout/wlogout.nix
    ../waybar/waybar.nix
    ../dunst/dunst.nix
    ../rofi/rofi.nix
    ../eww/eww.nix
    ../copyq/copyq.nix
    ../flameshot/flameshot.nix
  ];
  wayland.windowManager.sway = {
    enable = true;
    # extraConfig = "
    # blur
    # enable corner_radius 5
    # default_dim_inactive 0.8";
    config = {
      modifier = "Mod4";
      terminal = "kitty";
      menu = "rofi -show drun";
      gaps = {
        inner = 10;
        outer = 15;
      };
      input = {
        "keyboard" = {
          repeat_delay = "150";
          repeat_rate = "60";
        };
      };
      keybindings = let
        modifier = config.wayland.windowManager.sway.config.modifier;
      in
        lib.mkOptionDefault {
          "${modifier}+e" = "exec kitty -e nvim";

          "${modifier}+b" = "exec brave";
          "${modifier}+q" = "kill";
          "${modifier}+escape" = "exec wlogout";
          "${modifier}+n" = "exec nautilus";
          "${modifier}+v" = null;

          "${modifier}+Shift+s" = "exec flameshot gui";
          "${modifier}+p" = "exec wdisplays";
          "ctrl+Shift+l" = "exec swaylock";

          "XF86AudioRaiseVolume" = "exec pamixer -i 5";

          "XF86AudioLowerVolume" = "exec pamixer -d 5";

          "XF86MonBrightnessUp" = "exec brightnessctl s +10%";

          "XF86MonBrightnessDown" = "exec brightnessctl s 10%-";
        };
      startup = [
        {
          command = "dbus-sway-environment";
        }
        {command = "configure-gtk";}
        {
          command = "waybar";
        }
        {
          command = "nm-applet";
        }
        {
          command = "swww init";
        }
        {
          command = "copyq";
        }
        {
          command = "blueman-applet";
        }
        {
          command = "autotiling";
        }
        {
          command = "wlsunset -S 8:30 -s 19:30";
        }
      ];
    };
    extraOptions = ["--unsupported-gpu"];
    extraSessionCommands = ''
        export
        SDL_VIDEODRIVER=wayland
      # needs qt5.qtwayland in systemPackages
      export QT_QPA_PLATFORM=wayland
      export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
      # Fix for some Java AWT applications (e.g. Android Studio),
      # use this if they aren't displayed properly:
      export _JAVA_AWT_WM_NONREPARENTING=1
      export XDG_CURRENT_DESKTOP=sway
      export XDG_SESSION_DESKTOP=sway
    '';
  };
}
