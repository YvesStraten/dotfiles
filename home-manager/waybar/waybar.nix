{
  lib,
  options,
  config,
  ...
}:
let
  cfg = config.custom.waybar;
  inherit (lib) mkEnableOption mkIf;
in
{
  options.custom.waybar.enable = mkEnableOption "Enable waybar";

  config = mkIf cfg.enable {
    programs.waybar = {
      enable = true;
      systemd.enable = true;
      settings = {
        mainBar = {
          layer = "top";
          position = "top";
          mod = "dock";
          exclusive = true;
          passthrough = false;
          gtk-layer-shell = true;
          height = 0;
          modules-left = [
            "custom/weather"
            "idle_inhibitor"
            "hyprland/workspaces"
            "hyprland/window"
          ];
          modules-right = [
            "tray"
            "battery"
            "backlight"
            "pulseaudio"
            "pulseaudio#microphone"
            "clock"
          ];

          "hyprland/window" = {
            format = "{}";
          };
          "hyprland/workspaces" = {
            disable-scroll = true;
            all-outputs = true;
            on-click = "activate";
            format = "{name}: {icon}";
            format-icons = {
              "1" = "";
              "2" = "󰪶";
              "3" = "";
              "4" = "";
              "5" = " ";
              "6" = " ";
              "7" = " ";
              "8" = " ";
              "9" = "󰊶";
            };
            persistent_workspaces = {
              "*" = 9;
            };
          };

          "custom/weather" = {
            tooltip = true;
            format = "{}";
            interval = 30;
            exec = "~/.config/waybar/scripts/waybar-wttr.py";
            return-type = "json";
          };

          "tray" = {
            icon-size = 20;
            spacing = 10;
          };

          "clock" = {
            format = " {:%R   %d/%m}";
            tooltip-format = "<tt><small>{calendar}</small></tt>";
            calendar = {
              mode = "month";
              mode-mon-col = 3;
              weeks-pos = "right";
              on-scroll = 1;
              on-click-right = "mode";
              format = {
                months = "<span color='#ffead3'><b>{}</b></span>";
                days = "<span color='#ecc6d9'><b>{}</b></span>";
                weeks = "<span color='#99ffdd'><b>W{}</b></span>";
                weekdays = "<span color='#ffcc66'><b>{}</b></span>";
                today = "<span color='#ff6699'><b><u>{}</u></b></span>";
              };
            };
            actions = {
              on-click-right = "mode";
              on-click-forward = "tz_up";
              on-click-backward = "tz_down";
              on-scroll-up = "shift_up";
              on-scroll-down = "shift_down";
            };
          };

          "backlight" = {
            device = "intel_backlight";
            format = "{icon}   {percent}%";
            format-icons = [
              "󰃞"
              "󰃟"
              "󰃠"
            ];
            on-scroll-up = "brightnessctl set 1%+";
            on-scroll-down = "brightnessctl set 1%-";
            min-length = 6;
          };

          "battery" = {
            states = {
              good = 95;
              warning = 30;
              critical = 10;
            };
            format = "{icon}   {capacity}%";
            format-charging = "   {capacity}%";
            format-plugged = "  {capacity}%";
            format-alt = "{time} {icon}";
            format-icons = [
              ""
              ""
              ""
              ""
              ""
            ];
          };

          "pulseaudio" = {
            format = "{icon}   {volume}%";
            tooltip = false;
            on-click = "pamixer -t";
            format-muted = " Muted";
            on-scroll-up = "pamixer -i 5";
            on-scroll-down = "pamixer -d 5";
            scroll-step = 5;
            format-icons = {
              headphone = "";
              hands-free = "";
              headset = "";
              phone = "";
              portable = "";
              car = "";
              default = [
                ""
                ""
                ""
              ];
            };
          };

          "pulseaudio#microphone" = {
            format = "{format_source}";
            format-source = " {volume}%";
            format-source-muted = " Muted";
            on-click = "pamixer --default-source -t";
            on-scroll-up = "pamixer --default-source -i 5";
            on-scroll-down = "pamixer --default-source -d 5";
            scroll-step = 5;
          };

          "cpu" = {
            interval = 5;
            on-click = "'(proced)'";
            format = "  {usage:2}%";
          };
        };
      };

      style = ''
        * {
            font-family: FontAwesome, Roboto, Helvetica, Arial, sans-serif;
            font-size: 14px;
        }

        window#waybar {
            background-color: rgba(43, 48, 59, 0.5);
            border-bottom: 3px solid rgba(100, 114, 125, 0.5);
            color: #ffffff;
            transition-property: background-color;
            transition-duration: .5s;
        }

        window#waybar.hidden {
            opacity: 0.2;
        }

        /*
        window#waybar.empty {
            background-color: transparent;
        }
        window#waybar.solo {
            background-color: #FFFFFF;
        }
        */

        window#waybar.termite {
            background-color: #3F3F3F;
        }

        window#waybar.chromium {
            background-color: #000000;
            border: none;
        }

        button {
            /* Use box-shadow instead of border so the text isn't offset */
            box-shadow: inset 0 -3px transparent;
            /* Avoid rounded borders under each button name */
            border: none;
            border-radius: 0;
        }

        /* https://github.com/Alexays/Waybar/wiki/FAQ#the-workspace-buttons-have-a-strange-hover-effect */
        button:hover {
            background: inherit;
            box-shadow: inset 0 -3px #ffffff;
        }

        #workspaces button {
            padding: 0 5px;
            background-color: transparent;
            color: #ffffff;
        }

        #workspaces button:hover {
            background: rgba(0, 0, 0, 0.2);
        }

        #workspaces button.focused {
            background-color: #64727D;
            box-shadow: inset 0 -3px #ffffff;
        }

        #workspaces button.active {
            box-shadow: inset 0 -3px #ffffff;
         }

        #workspaces button.urgent {
            background-color: #eb4d4b;
        }

        #mode {
            background-color: #64727D;
            border-bottom: 3px solid #ffffff;
        }

        #clock,
        #battery,
        #cpu,
        #memory,
        #disk,
        #temperature,
        #backlight,
        #network,
        #pulseaudio,
        #wireplumber,
        #custom-media,
        #tray,
        #mode,
        #idle_inhibitor,
        #scratchpad,
        #mpd {
            padding: 0 15px;
            color: #ffffff;
        }

        #window,
        #workspaces {
            margin: 0 4px;
        }

        /* If workspaces is the leftmost module, omit left margin */
        .modules-left > widget:first-child > #workspaces {
            margin-left: 0;
        }

        /* If workspaces is the rightmost module, omit right margin */
        .modules-right > widget:last-child > #workspaces {
            margin-right: 0;
        }

        #battery {
            background-color: #ffffff;
            color: #000000;
        }

        #battery.charging, #battery.plugged {
            color: #ffffff;
            background-color: #26A65B;
        }

        @keyframes blink {
            to {
                background-color: #ffffff;
                color: #000000;
            }
        }

        #battery.critical:not(.charging) {
            background-color: #f53c3c;
            color: #ffffff;
            animation-name: blink;
            animation-duration: 0.5s;
            animation-timing-function: linear;
            animation-iteration-count: infinite;
            animation-direction: alternate;
        }

        label:focus {
            background-color: #000000;
        }

        #cpu {
            background-color: #2ecc71;
            color: #000000;
        }

        #memory {
            background-color: #9b59b6;
        }

        #disk {
            background-color: #964B00;
        }

        #backlight {
            background-color: #90b1b1;
        }

        #network {
            background-color: #2980b9;
        }

        #network.disconnected {
            background-color: #f53c3c;
        }

        #pulseaudio {
            background-color: #f1c40f;
            color: #000000;
        }

        #pulseaudio.muted {
            background-color: #90b1b1;
            color: #2a5c45;
        }

        #wireplumber {
            background-color: #fff0f5;
            color: #000000;
        }

        #wireplumber.muted {
            background-color: #f53c3c;
        }

        #custom-media {
            background-color: #66cc99;
            color: #2a5c45;
            min-width: 100px;
        }

        #custom-media.custom-spotify {
            background-color: #66cc99;
        }

        #custom-media.custom-vlc {
            background-color: #ffa000;
        }

        #temperature {
            background-color: #f0932b;
        }

        #temperature.critical {
            background-color: #eb4d4b;
        }

        #tray {
            background-color: #2980b9;
        }

        #tray > .passive {
            -gtk-icon-effect: dim;
        }

        #tray > .needs-attention {
            -gtk-icon-effect: highlight;
            background-color: #eb4d4b;
        }

        #idle_inhibitor {
            background-color: #2d3436;
        }

        #idle_inhibitor.activated {
            background-color: #ecf0f1;
            color: #2d3436;
        }

        #mpd {
            background-color: #66cc99;
            color: #2a5c45;
        }


        #mpd.disconnected {
            background-color: #f53c3c;
        }

        #mpd.stopped {
            background-color: #90b1b1;
        }

        #mpd.paused {
            background-color: #51a37a;
        }

        #language {
            background: #00b093;
            color: #740864;
            padding: 0 5px;
            margin: 0 5px;
            min-width: 16px;
        }

        #keyboard-state {
            background: #97e1ad;
            color: #000000;
            padding: 0 0px;
            margin: 0 5px;
            min-width: 16px;
        }

        #keyboard-state > label {
            padding: 0 5px;
        }

        #keyboard-state > label.locked {
            background: rgba(0, 0, 0, 0.2);
        }

        #scratchpad {
            background: rgba(0, 0, 0, 0.2);
        }

        #scratchpad.empty {
          background-color: transparent;
        }
      '';
    };

    home.file.".config/waybar/scripts" = {
      source = ./scripts;
      recursive = true;
    };
  };
}
