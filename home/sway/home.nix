{ config
, pkgs
, lib
, ...
}: {
  wayland.windowManager.sway =
    {
      enable = true;
      config =
        {
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
          keybindings =
            let modifier = config.wayland.windowManager.sway.config.modifier;
            in lib.mkOptionDefault {

              "${modifier}+e" = "exec emacsclient -c";

              "${modifier}+b" = "exec brave";
              "${modifier}+q" = "kill";
              "${modifier}+escape" = "wlogout";

              "${modifier}+Shift+s" = "exec flameshot gui";
              "${modifier}+p" = "exec wdisplays";
              "ctrl+l" = "exec swaylock";

              "XF86AudioRaiseVolume" = "exec pamixer -i 5";

              "XF86AudioLowerVolume" = "exec pamixer -d 5";

              "XF86MonBrightnessUp" = "exec brightnessctl s +10%";

              "XF86MonBrightnessDown" = "exec brightnessctl s 10%-";
            };
          startup = [
            {
              command = "dbus-sway-environment";
            }
            { command = "configure-gtk"; }
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
            {
              command = "swayidle -w timeout 300 'swaylock -f' timeout 480 'hyprctl dispatch dpms off' resume 'hyprctl dpms on' before-sleep 'swaylock -f'";
            }
          ];
        };
      extraOptions = [ "--unsupported-gpu" ];
      extraSessionCommands = ''
        export SDL_VIDEODRIVER=wayland
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
