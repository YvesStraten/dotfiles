{ config
, pkgs
, ...
}: {
  home = {
    sessionVariables = {
      GRIMBLAST_EDITOR = "gwenview";
      _JAVA_AWT_WM_NONREPARENTING = "1";
      XCURSOR_SIZE = "24";
      WLR_NO_HARDWARE_CURSORS = "1";
      NIXOS_OZONE_WL = "1";
    };
  };

  home.file.".config/hypr/scripts" = {
    source = ./scripts;
    recursive = true;
  };

  wayland.windowManager.hyprland =
    {
      enable = true;
      systemdIntegration = true;
      extraConfig = ''
        exec-once = waybar
        exec-once = nm-applet
        exec-once = wlsunset -S 8:30 -s 19:30
        exec-once = plasma-browser-integration-host
        exec-once = blueman-applet
        exec-once = swayidle -w timeout 300 'swaylock -f' timeout 480 'hyprctl dispatch dpms off' resume 'hyprctl dpms on' before-sleep 'swaylock -f'
        # exec-once = swaynag-battery
        exec-once = ~/.config/hypr/scripts/wallpaper.sh

        monitor = ,highrr,auto,1

        input {
        kb_layout = us
        repeat_rate = 40
        repeat_delay = 400
        follow_mouse = 1
        sensitivity = 0 # -1.0 - 1.0, 0 means no modification.
        }

        bind = SUPER, B, exec, brave 
        bind = SUPER, F1, exec, ~/.config/hypr/scripts/keybind
        bind = SUPER, W, exec, ~/.config/hypr/scripts/monitors
        bind = , XF86AudioRaiseVolume, exec, pamixer -i 5 
        bind = , XF86AudioLowerVolume, exec, pamixer -d 5 
        bind = , XF86MonBrightnessUp, exec, brightnessctl s +10%
        bind = , XF86MonBrightnessDown, exec, brightnessctl s 10%-

        $screenshotarea = hyprctl keyword animation "fadeOut,0,0,default"; grimblast --notify copysave area ~/Pictures/$(date +%d-%m-%Y-%M-%S).png; "fadeOut,1,4,default"
        $editarea = hyprctl keyword animation "fadeOut,0,0,default"; grimblast --notify edit area ~/Pictures/$(date +%d-%m-%Y-%M-%S).png; "fadeOut,1,4,default" 
        bind = SUPER SHIFT, S, exec, $screenshotarea
        bind = , Print, exec, grimblast --notify --cursor copysave output
        bind = ALT, Print, exec, grimblast --notify --cursor copysave screen
        bind = ALT SUPER, S, exec, $editarea 
        # bind=,Print,exec,grim $(xdg-user-dir PICTURES)/Screenshots/$(date +'%Y%m%d%H%M%S_1.png') && notify-send 'Screenshot Saved'
        # bind=SUPER,Print,exec,grim - | wl-copy && notify-send 'Screenshot Copied to Clipboard'
        # bind=SUPERSHIFT,Print,exec,grim - | swappy -f -
        # bind=SUPERSHIFT,S,exec,slurp | grim -g - $(xdg-user-dir PICTURES)/Screenshots/$(date +'%Y%m%d%H%M%S_1.png') && notify-send 'Screenshot Saved'

        bind = SUPER SHIFT, X, exec, hyprpicker -a -n
        bind = CTRL ALT, L, exec, swaylock
        bind = SUPER, Return, exec, kitty
        bind = SUPER, X, exec, kitty
        bind = SUPER, E, exec, emacsclient -c 
        bind = SUPER, N, exec, nautilus
        bind = SUPER, R, exec, killall rofi || rofi -show drun
        bind = SUPER, escape, exec, wlogout --protocol layer-shell -b 5 -T 400 -B 400

        general {
        gaps_in=10
        gaps_out=15
        no_border_on_floating = true
        layout = dwindle

        col.active_border = rgb(44475a) rgb(bd93f9) 90deg
        col.inactive_border = rgba(44475aaa)
        col.group_border = rgba(282a36dd)
        col.group_border_active = rgb(bd93f9) rgb(44475a) 90deg
          # non-gradient alternative
          #col.active_border = rgb(bd93f9)
          #col.inactive_border = rgba(44475aaa)
          #col.group_border = rgba(282a36dd)
          #col.group_border_active = rgb(bd93f9)
          # darker alternative
          #col.active_border = rgb(44475a) # or rgb(6272a4)
          #col.inactive_border = rgb(282a36)
          #col.group_border = rgb(282a36)
          #col.group_border_active = rgb(44475a) # or rgb(6272a4)

        }

        decoration {
           # Rounded windows
           rounding = 8
           multisample_edges = true

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
        bezier = overshot, 0.05, 0.9, 0.1, 1.05
        bezier = smoothOut, 0.36, 0, 0.66, -0.56
        bezier = smoothIn, 0.25, 1, 0.5, 1

        animation = windows, 1, 5, overshot, slide
        animation = windowsOut, 1, 4, smoothOut, slide
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
        windowrule = float, Lxappearance
        windowrule = float, Rofi
        windowrule = animation none,Rofi
        windowrule = float,viewnior
        windowrule = float,feh
        windowrule = float, pavucontrol-qt
        windowrule = float, pavucontrol
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
        windowrule = tile, neovide

        # Where applications appear
        windowrule = workspace 8, Kuro
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
