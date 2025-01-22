{ pkgs, inputs, ... }:
{
  imports = [
    ../wlogout/wlogout.nix
    ../waybar/waybar.nix
    ../dunst/dunst.nix
    ../rofi/rofi.nix
    ../eww/eww.nix
    ../swappy/swappy.nix
  ];

  programs.hyprland = {
    enable = true;
    package = inputs.hyprland.packages.${pkgs.system}.hyprland;
    portalPackage = inputs.hyprland.${pkgs.system}.xdg-desktop-portal-hyprland;
    xwayland.enable = true;
    withUWSM = true;
  };

  services.xserver.displayManager.sddm = {
    enable = true;
    theme = "${pkgs.yvess.sugar-dark}";
  };

  sound.enable = false;
  security.rtkit.enable = true;
  security.polkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  security.pam.services.hyprlock = {}; 

  hm = {
    home = {
      sessionVariables = {
        _JAVA_AWT_WM_NONREPARENTING = "1";
        XCURSOR_SIZE = "30";
        NIXOS_OZONE_WL = "1";
      };

      packages = with pkgs; [
        yvess.wall-switch
        wl-clipboard
        swww
        hyprlock
        hypridle
      ];
    };

    programs.hyprlock = {
      enable = true;
      extraConfig = ''
        general {
            lock_cmd = pidof hyprlock || hyprlock       # avoid starting multiple hyprlock instances.
            before_sleep_cmd = loginctl lock-session    # lock before suspend.
            after_sleep_cmd = hyprctl dispatch dpms on  # to avoid having to press a key twice to turn on the display.
        }

        listener {
            timeout = 150                                # 2.5min.
            on-timeout = brightnessctl -s set 10         # set monitor backlight to minimum, avoid 0 on OLED monitor.
            on-resume = brightnessctl -r                 # monitor backlight restore.
        }

        # turn off keyboard backlight, comment out this section if you dont have a keyboard backlight.
        listener {
            timeout = 150                                          # 2.5min.
            on-timeout = brightnessctl -sd rgb:kbd_backlight set 0 # turn off keyboard backlight.
            on-resume = brightnessctl -rd rgb:kbd_backlight        # turn on keyboard backlight.
        }

        listener {
            timeout = 300                                 # 5min
            on-timeout = loginctl lock-session            # lock screen when timeout has passed
        }

        listener {
            timeout = 330                                 # 5.5min
            on-timeout = hyprctl dispatch dpms off        # screen off when timeout has passed
            on-resume = hyprctl dispatch dpms on          # screen on when activity is detected after timeout has fired.
        }

        listener {
            timeout = 1800                                # 30min
            on-timeout = systemctl suspend                # suspend pc
        }
      '';
    };

    services = {
      kdeconnect = {
        enable = true;
        indicator = true;
      };

      udiskie = {
        enable = true;
        automount = true;
        tray = "never";
      };

      cliphist = {
        enable = true; 
        allowImages = true;
      };
    };

    wayland.windowManager.hyprland = {
      enable = true;
      systemd.enable = false;
      settings = {
        "$mod" = "alt";
        bind = [
        ] ++ (
         # workspaces
        # binds $mod + [shift +] {1..9} to [move to] workspace {1..9}
        builtins.concatLists (builtins.genList (i:
            let ws = i + 1;
            in [
              "$mod, code:1${toString i}, workspace, ${toString ws}"
              "$mod SHIFT, code:1${toString i}, movetoworkspace, ${toString ws}"
              "$mod SHIFT, code:1${toString i}, movetoworkspace, ${toString ws}"
            ]
          )
          9) 
        );
      };
      extraConfig = with pkgs; ''
        $mod = "alt"
        exec-once = waybar
        exec-once = hypridle
        exec-once = ${networkmanagerapplet}/bin/nm-applet

        exec-once = plasma-browser-integration-host
        exec-once = ${blueman}/bin/blueman-applet

        exec-once = swww init
        monitor = ,highrr,auto,1

        input {
        kb_layout = us
        repeat_rate = 40
        repeat_delay = 400
        follow_mouse = 1
        sensitivity = 0 # -1.0 - 1.0, 0 means no modification.
        touchpad {
                 natural_scroll = true
                 clickfinger_behavior = true
        }
        }

        gestures {
                 workspace_swipe = true
                 workspace_swipe_fingers = 3
                 workspace_swipe_numbered = true
        }

        bind = $mod, B, exec, firefox
        bind = $mod, F1, exec, ~/.config/hypr/scripts/keybind

        bind = , XF86AudioRaiseVolume, exec, ${pamixer}/bin/pamixer -i 5
        bind = , XF86AudioLowerVolume, exec, ${pamixer}/bin/pamixer -d 5
        bind = , XF86MonBrightnessUp, exec, ${brightnessctl}/bin/brightnessctl s +10%
        bind = , XF86MonBrightnessDown, exec, ${brightnessctl}/bin/brightnessctl s 10%-


        bind = SUPER SHIFT, S, exec, ${grim}/bin/grim -g "$(${slurp}/bin/slurp)" - | ${swappy}/bin/swappy -f -

        bind = SUPER SHIFT, X, exec, ${hyprpicker}/bin/hyprpicker | ${wl-clipboard}/bin/wl-copy
        bind = $mod SHIFT, L, exec, hyprlock
        bind = $mod, Return, exec, ${kitty}/bin/kitty
        bind = $mod, E, exec, emacs
        bind = $mod, N, exec, yazi
        bind = $mod, R, exec, killall rofi || rofi -show drun
        bind = $mod, V, exec, ${cliphist}/bin/cliphist list | wofi --show dmenu | ${cliphist}/bin/cliphist decode | ${wl-clipboard}/bin/wl-copy
        bind = $mod, escape, exec, ${wlogout}/bin/wlogout --protocol layer-shell -b 5 -T 400 -B 400
        bind = $mod, period, exec, rofi -modi emoji -show emoji --action copy

        general {
                gaps_in=10
                gaps_out=15
                no_border_on_floating = true
                # allow_tearing = true
                layout = dwindle

        }

        # env = WLR_DRM_NO_ATOMIC,1

        decoration {
           # Rounded windows
           rounding = 10

           #Opacity
           active_opacity = 1.0
           inactive_opacity = 1.0

           col.shadow = rgba(1E202966)
           drop_shadow = yes
           shadow_range = 60
           shadow_offset = 1 2
           shadow_render_power = 3
            shadow_scale = 0.97 #shadows


           blurls = gtk-layer-shell
           # blurls = waybar
           blurls = lockscreen
        }

        animations {
                   enabled = true

                   # Curves
                   bezier = smooth, 0, 0, 0.95, 0.44
                   bezier = smoothOut, 0.36, 0, 0.66, -0.56
                   bezier = smoothIn, 0.25, 1, 0.5, 1

                   animation = windows, 1, 5, smooth, slide
                   animation = windowsOut, 1, 4, smooth, slide
                   animation = windowsMove, 1, 4, default
                   animation = border, 1, 10, default
                   animation = fade, 1, 10, smoothIn
                   animation = fadeDim, 1, 10, smoothIn
                   animation = workspaces, 1, 6, default
        }

        dwindle {
          no_gaps_when_only = false
          pseudotile = true # master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
          preserve_split = true # you probably want this
        }

        bind = $mod, Q, killactive,
        bind = $mod, F, fullscreen,
        bind = $mod, Space, togglefloating,
        bind = $mod, I, pseudo, # dwindle
        bind = $mod, S, togglesplit, # dwindle

        bind = $mod, h, movefocus, l
        bind = $mod, l, movefocus, r
        bind = $mod, j, movefocus, u
        bind = $mod, k, movefocus, d

        bind = $mod SHIFT, h, movewindow, l
        bind = $mod SHIFT, l, movewindow, r
        bind = $mod SHIFT, k, movewindow, u
        bind = $mod SHIFT, j, movewindow, d

        bind = $mod CTRL, h, resizeactive, -20 0
        bind = $mod CTRL, l, resizeactive, 20 0
        bind = $mod CTRL, j, resizeactive, 0 -20
        bind = $mod CTRL, k, resizeactive, 0 20

        bind = SUPER ALT, up, workspace, e+1
        bind = SUPER ALT, down, workspace, e-1

        bind = $mod, g, togglegroup
        bind = $mod, tab, changegroupactive

        bind = $mod, grave, togglespecialworkspace
        bind = SUPERSHIFT, grave, movetoworkspace, special

        windowrule = float, file_progress
        windowrule = float, blueman-manager
        windowrule = float, confirm
        windowrule = float, dialog
        windowrule = float, download
        windowrule = float, notification
        windowrule = float, error
        windowrule = float, splashhyprl
        windowrule = float, confirmreset
        windowrule = float, title:Open File
        windowrule = float, title:branchdialog
        windowrule = float, Rofi
        windowrule = animation none,Rofi
        windowrule = float,viewnior
        windowrule = float,feh
        windowrule = float, org.gnome.Nautilus
        windowrule = size 1071 658, org.gnome.Nautilus
        windowrule = float, pavucontrol-qt
        windowrule = float, pavucontrol
        windowrule = float, whatsapp-for-linux
        windowrule = pin, whatsapp-for-linux
        windowrule = float, file-roller
        windowrule = fullscreen, wlogout
        windowrule = float, title:wlogout
        windowrule = fullscreen, title:wlogout
        windowrule = fullscreen, dunst
        windowrule = idleinhibit focus, mpv
        windowrule = idleinhibit lutris
        windowrule = idleinhibit steam
        windowrule = idleinhibit fullscreen, brave
        windowrule = float, title:^(Media viewer)$
        windowrule = float, title:^(Volume Control)$
        windowrule = float, title:^(Picture-in-Picture)$
        windowrule = size 800 600, title:^(Volume Control)$
        windowrule = move 75 44%, title:^(Volume Control)$
        # windowrulev2 = immediate, title:^(Heroic Games Launcher)$

        # Where applications appear
        windowrule = workspace 9, rclone-browser

        misc {
             disable_hyprland_logo = true
             disable_splash_rendering = true
             mouse_move_enables_dpms = true
             enable_swallow = true
             swallow_regex = ^(wezterm)$
        }

        bindm = $mod, mouse:272, movewindow
        bindm = $mod, mouse:273, resizewindow
        bind = $mod, mouse_down, workspace, e+1
        bind = $mod, mouse_up, workspace, e-1
      '';
    };
  };
}
