{
  config,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    inputs.hyprland.homeManagerModules.default
    ../swaylock/swaylock.nix
    ../wlogout/wlogout.nix
    ../waybar/waybar.nix
    ../dunst/dunst.nix
    ../rofi/rofi.nix
    ../eww/eww.nix
    ../swappy/swappy.nix
  ];
  home = {
    sessionVariables = {
      _JAVA_AWT_WM_NONREPARENTING = "1";
      XCURSOR_SIZE = "30";
      WLR_NO_HARDWARE_CURSORS = "1";
      NIXOS_OZONE_WL = "1";
    };

    packages = with pkgs; [
      yvess.wall-switch
      swww
      nwg-displays
    ];
  };

  home.file.".config/hypr/scripts" = {
    source = ./scripts;
    recursive = true;
  };

  services = {
    kdeconnect = {
      enable = true;
      indicator = true;
    };
  };

  wayland.windowManager.hyprland = {
    enable = true;
    systemdIntegration = true;
    extraConfig = with pkgs; ''
                  source = ~/.config/hypr/monitors.conf
                  exec-once = waybar
                  exec-once = ${networkmanagerapplet}/bin/nm-applet
                  exec-once = ${wlsunset}/bin/wlsunset -S 8:30 -s 19:30
                  exec-once = ${xdg-desktop-portal-hyprland}/libexec/xdg-desktop-portal-hyprland
                  exec-once = plasma-browser-integration-host
                  exec-once = ${blueman}/bin/blueman-applet
                  exec-once = ${swayidle}/bin/swayidle -w timeout 300 '${swaylock-effects}/bin/swaylock -f' timeout 480 'hyprctl dispatch dpms off' resume 'hyprctl dpms on' before-sleep '${swaylock-effects}/bin/swaylock -f'
      exec-once = ${wl-clipboard}/bin/wl-paste --type text --watch ${cliphist}/bin/cliphist store #Stores only text data

      exec-once = ${wl-clipboard}/bin/wl-paste --type image --watch ${cliphist}/bin/cliphist store #Stores only image data
                  exec-once = swww init
                  exec-once = ~/.config/hypr/scripts/wall.sh
                  exec = ~/.config/hypr/scripts/wall.sh
                  exec-once = ${polkit_gnome}/libexec/polkit-gnome-authentication-agent-1

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


                  bind = SUPER, B, exec, ${brave}/bin/brave
                  bind = SUPER, F1, exec, ~/.config/hypr/scripts/keybind
                  bind = SUPER, W, exec, ${alacritty}/bin/alacritty -e ~/.config/hypr/scripts/monitors.sh
                  bind = , XF86AudioRaiseVolume, exec, ${pamixer}/bin/pamixer -i 5
                  bind = , XF86AudioLowerVolume, exec, ${pamixer}/bin/pamixer -d 5
                  bind = , XF86MonBrightnessUp, exec, ${brightnessctl}/bin/brightnessctl s +10%
                  bind = , XF86MonBrightnessDown, exec, ${brightnessctl}/bin/brightnessctl s 10%-


                  bind = SUPER SHIFT, S, exec, ${grim}/bin/grim -g "$(${slurp}/bin/slurp)" - | ${swappy}/bin/swappy -f -

                  bind = SUPER SHIFT, X, exec, ${hyprpicker}/bin/hyprpicker | ${wl-clipboard}/bin/wl-copy
                  bind = CTRL ALT, L, exec, ${swaylock-effects}/bin/swaylock
                  bind = SUPER, Return, exec, ${alacritty}/bin/alacritty
                  bind = SUPER, X, exec, ${alacritty}/bin/alacritty
                  bind = SUPER, E, exec, ${emacs29-pgtk}/bin/emacs
                  bind = SUPER, N, exec, ${gnome.nautilus}/bin/nautilus
                  bind = SUPER, R, exec, killall rofi || rofi -show drun
                  bind = SUPER, V, exec, ${cliphist}/bin/cliphist list | rofi -dmenu | ${cliphist}/bin/cliphist decode | ${wl-clipboard}/bin/wl-copy
                  bind = SUPER, escape, exec, ${wlogout}/bin/wlogout --protocol layer-shell -b 5 -T 400 -B 400
                  bind = SUPER, period, exec, rofi -modi emoji -show emoji --action copy

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


                     blurls =
                     gtk-layer-shell
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

                  bind = SUPER, Q, killactive,
                  bind = SUPER, F, fullscreen,
                  bind = SUPER, Space, togglefloating,
                  bind = SUPER, I, pseudo, # dwindle
                  bind = SUPER, S, togglesplit, # dwindle

                  bind = SUPER, h, movefocus, l
                  bind = SUPER, l, movefocus, r
                  bind = SUPER, j, movefocus, u
                  bind = SUPER, k, movefocus, d

                  bind = SUPER SHIFT, h, movewindow, l
                  bind = SUPER SHIFT, l, movewindow, r
                  bind = SUPER SHIFT, k, movewindow, u
                  bind = SUPER SHIFT, j, movewindow, d

                  bind = SUPER CTRL, h, resizeactive, -20 0
                  bind = SUPER CTRL, l, resizeactive, 20 0
                  bind = SUPER CTRL, j, resizeactive, 0 -20
                  bind = SUPER CTRL, k, resizeactive, 0 20

                  bind = SUPER SHIFT, 1, movetoworkspace, 1
                  bind = SUPER SHIFT, 2, movetoworkspace, 2
                  bind = SUPER SHIFT, 3, movetoworkspace, 3
                  bind = SUPER SHIFT, 4, movetoworkspace, 4
                  bind = SUPER SHIFT, 5, movetoworkspace, 5
                  bind = SUPER SHIFT, 6, movetoworkspace, 6
                  bind = SUPER SHIFT, 7, movetoworkspace, 7
                  bind = SUPER SHIFT, 8, movetoworkspace, 8
                  bind = SUPER SHIFT, 9, movetoworkspace, 9
                  bind = SUPER SHIFT, 0, movetoworkspace, 10

                  bind = SUPER, 1, workspace, 1
                  bind = SUPER, 2, workspace, 2
                  bind = SUPER, 3, workspace, 3
                  bind = SUPER, 4, workspace, 4
                  bind = SUPER, 5, workspace, 5
                  bind = SUPER, 6, workspace, 6
                  bind = SUPER, 7, workspace, 7
                  bind = SUPER, 8, workspace, 8
                  bind = SUPER, 9, workspace, 9
                  bind = SUPER, 0, workspace, 10
                  bind = SUPER ALT, up, workspace, e+1
                  bind = SUPER ALT, down, workspace, e-1

                  bind = SUPER, g, togglegroup
                  bind = SUPER, tab, changegroupactive

                  bind = SUPER, grave, togglespecialworkspace
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

                  bindm = SUPER, mouse:272, movewindow
                  bindm = SUPER, mouse:273, resizewindow
                  bind = SUPER, mouse_down, workspace, e+1
                  bind = SUPER, mouse_up, workspace, e-1
    '';
  };

  services.udiskie = {
    enable = true;
    automount = true;
    tray = "never";
  };
}
