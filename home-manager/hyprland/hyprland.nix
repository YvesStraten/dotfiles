{
  config,
  options,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.custom.hyprland;
  inherit (lib) mkEnableOption mkIf mkMerge;
in
{
  options.custom.hyprland.enable = mkEnableOption "Enable hyprland";

  config = mkMerge [
    (mkIf cfg.enable {
      home = {
        sessionVariables = {
          NIXOS_OZONE_WL = "1";
        };

        packages = with pkgs; [
          blueman
          yvess.wall-switch
          wl-clipboard
          swww
          hypridle
          kdePackages.gwenview
          swappy
          pavucontrol
          selectdefaultapplication
        ];
      };

      custom = {
        swappy.enable = true;
        udisks.enable = true;
        rofi.enable = true;
        # wlogout.enable = true;
        kanshi.enable = true;
        # nwg-dock.enable = true;
        theming.enable = true;
      };

      programs.dankMaterialShell = {
        enable = true;
        systemd.enable = true;
      };

      programs.hyprlock = {
        enable = false;
        settings = {
          general = {
            disable_loading_bar = true;
            no_fade_in = false;
          };

          background = [
            {
              path = "~/.cache/lock-image";
            }
          ];

          input-field = [
            {
              size = "200, 50";
              position = "0, -80";
              monitor = "";
              dots_center = true;
              fade_on_empty = false;
              font_color = "rgb(202, 211, 245)";
              inner_color = "rgb(91, 96, 120)";
              outer_color = "rgb(24, 25, 38)";
              outline_thickness = 5;
              placeholder_text = "<span foreground=\"##cad3f5\">Password...</span>";
              shadow_passes = 2;
            }
          ];
        };
      };

      systemd.user.services = {
        poweralertd.Unit.After = lib.mkForce [ "graphical-session.target" ];
        polkit-gnome-authentication-agent-1 = {
          Unit = {
            Description = "polkit-gnome-authentication-agent-1";
            Wants = [ "graphical-session.target" ];
            After = [ "graphical-session.target" ];
          };
          Install = {
            WantedBy = [ "graphical-session.target" ];
          };
          Service = {
            Type = "simple";
            ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
            Restart = "on-failure";
            RestartSec = 1;
            TimeoutStopSec = 10;
          };
        };
      };

      services = {
        cliphist = {
          enable = true;
          allowImages = true;
        };

        poweralertd.enable = true;

        hypridle = {
          enable = true;
          settings = {
            listener =
              let
                brightnessctl = lib.getExe pkgs.brightnessctl;
              in
              [
                {
                  timeout = 60;
                  on-timeout = "${brightnessctl} -s set 10";
                  on-resume = "${brightnessctl} -r";
                }

                {
                  timeout = 90;
                  on-timeout = "loginctl lock-session";
                }

                {
                  timeout = 300;
                  on-timeout = "hyprctl dispatch dpms off";
                  on-resume = "hyprctl dispatch dpms on";
                }
              ];
          };
        };
      };

      wayland.windowManager.hyprland = {
        enable = true;
        systemd.enable = false;
        package = null;
        portalPackage = null;
        settings =
          let
            launcher = "${pkgs.app2unit}/bin/app2unit";
            uwsm-launch = program: "${launcher} -- ${program}";
            keys_directions = [
              {
                key = "h";
                direction = "l";
              }

              {
                key = "j";
                direction = "d";
              }

              {
                key = "k";
                direction = "u";
              }

              {
                key = "l";
                direction = "r";
              }
            ];
          in
          {
            "$mod" = "super";

            exec-once = [
              "${pkgs.systemd}/bin/systemctl --user restart gamemoded.service"
            ];

            bind =
              with pkgs;
              let
                screenshot = pkgs.writeShellScriptBin "screenshot" ''
                  set -euxo pipefail
                  ${grim}/bin/grim -g "$(${slurp}/bin/slurp)" - | ${swappy}/bin/swappy -f - -o "${config.xdg.userDirs.pictures}/Screenshots/swappy-$(date).png"
                '';
              in
              [
                "$mod, B, exec, ${uwsm-launch "firefox"}"
                ", XF86AudioRaiseVolume, exec, ${pamixer}/bin/pamixer -i 5"
                ", XF86AudioLowerVolume, exec, ${pamixer}/bin/pamixer -d 5"
                ", XF86MonBrightnessUp, exec, ${brightnessctl}/bin/brightnessctl s +5%"
                ", XF86MonBrightnessDown, exec, ${brightnessctl}/bin/brightnessctl s 5%-"
                "SUPER SHIFT, x, exec, ${uwsm-launch "${hyprpicker}/bin/hyprpicker | ${wl-clipboard}/bin/wl-copy"}"
                "$mod, Return, exec, ${uwsm-launch "${ghostty}/bin/ghostty"}"
                "$mod, N, exec, ${uwsm-launch "yazi.desktop"}"
                "$mod, R, exec, rofi -show drun -run-command '${launcher} -- {cmd}'"
                "$mod, Q, killactive,"
                "$mod, F, fullscreen,"
                "$mod SHIFT, Space, togglefloating,"
                "$mod, I, pseudo, # dwindle"
                "$mod, S, togglesplit, # dwindle"
                "$mod CTRL, h, resizeactive, -20 0"
                "$mod CTRL, l, resizeactive, 20 0"
                "$mod CTRL, j, resizeactive, 0 -20"
                "$mod CTRL, k, resizeactive, 0 20"
                "SUPER ALT, up, workspace, e+1"
                "SUPER ALT, down, workspace, e-1"
                "$mod, g, togglegroup"
                "$mod, tab, changegroupactive"
                "$mod, grave, togglespecialworkspace"
                "$mod SHIFT, grave, movetoworkspace, special"
                "$mod, v, exec, ${uwsm-launch "cliphist list | ${rofi}/bin/rofi -dmenu | cliphist decode | wl-copy"}"
                "$mod SHIFT, s, exec, ${uwsm-launch "${screenshot}/bin/screenshot"}"
              ]
              ++ (
                # workspaces
                # binds $mod + [shift +] {1..9} to [move to] workspace {1..9}
                builtins.concatLists (
                  builtins.genList (
                    i:
                    let
                      ws = i + 1;
                    in
                    [
                      "$mod, code:1${toString i}, workspace, ${toString ws}"
                      "$mod SHIFT, code:1${toString i}, movetoworkspace, ${toString ws}"
                    ]
                  ) 9
                )
              )
              ++ (builtins.concatLists (
                builtins.map (key_attr: [
                  "$mod, ${key_attr.key}, movefocus, ${key_attr.direction} "
                ]) keys_directions
              ))
              ++ (builtins.concatLists (
                builtins.map (key_attr: [
                  "$mod SHIFT, ${key_attr.key}, movewindow, ${key_attr.direction} "
                ]) keys_directions
              ));

            windowrule = [
              "match:class ^(xwaylandvideobridge)$, opacity 0, no_anim 1, no_initial_focus 1, max_size 1 1, no_blur 1, no_focus 1"
              "match:class ^(org.gnome.Nautilus)$, float 1"
              "match:class ^(Zotero|zotero)$, float 1"
              "match:class ^(Thunar|thunar)$, float 1"
              # make Firefox/Zen PiP window floating and sticky
              "match:title ^(Picture-in-Picture)$, float 1, pin 1"
              "match:class ^(firefox|mpv|.+exe)$, idle_inhibit fullscreen"
              "match:class .*, suppress_event maximize fullscreenoutput"
              "match:class ^(discord|Slack)$, workspace 6"
              "match:class steam, workspace 1"
            ];

            gesture = [
              "3, horizontal, workspace"
            ];
          };

        extraConfig = with pkgs; ''
          monitor = ,highrr,auto,auto

          input {
          kb_layout = us,se,de,it
          kb_options=grp:alt_shift_toggle
          repeat_rate = 40
          repeat_delay = 400
          follow_mouse = 1
          sensitivity = 0 # -1.0 - 1.0, 0 means no modification.
          touchpad {
                   natural_scroll = true
                   clickfinger_behavior = true
          }
          }

          bind = $mod, period, exec, rofi -modi emoji -show emoji

          general {
                  gaps_in=5
                  gaps_out=5
                  layout = dwindle

          }

          xwayland {
            force_zero_scaling = true

          }

          decoration {
                 rounding=8
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
            pseudotile = true # master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
            preserve_split = true # you probably want this
          }

          misc {
               disable_hyprland_logo = true
               disable_splash_rendering = true
               mouse_move_enables_dpms = true
               enable_swallow = true
               swallow_regex = ^(wezterm)$
               animate_manual_resizes = true
          }

          bindm = $mod, mouse:272, movewindow
          bindm = $mod, mouse:273, resizewindow
          bind = $mod, mouse_down, workspace, e+1
          bind = $mod, mouse_up, workspace, e-1
        '';
      };
    })
  ];
}
