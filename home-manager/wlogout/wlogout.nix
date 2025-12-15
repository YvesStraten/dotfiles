{
  config,
  options,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.custom.wlogout;
  inherit (lib) mkEnableOption mkIf;
in
{
  options.custom.wlogout.enable = mkEnableOption "Enable wlogout";

  config = mkIf cfg.enable {
    programs.wlogout = {
      enable = true;
      layout = [
        {
          label = "lock";
          action = "hyprlock";
          text = "Lock";
          keybind = "l";
        }

        {
          label = "reboot";
          action = "systemctl reboot";
          text = "Reboot";
          keybind = "r";
        }

        {
          label = "shutdown";
          action = "systemctl poweroff";
          text = "Shutdown";
          keybind = "s";
        }

        {
          label = "logout";
          action = "hyprctl dispatch exit 0";
          text = "Logout";
          keybind = "e";
        }

        {
          label = "suspend";
          action = "systemctl suspend";
          text = "Suspend";
          keybind = "u";
        }
      ];

      style =
        let
          icon-location = "${pkgs.wlogout}/share/wlogout/icons";
        in
        ''
          window {
              font-family: monospace;
              font-size: 14pt;
              color: #cdd6f4; /* text */
              background-color: rgba(30, 30, 46, 0.5);
          }

          button {
              background-repeat: no-repeat;
              background-position: center;
              background-size: 25%;
              border: none;
              background-color: rgba(30, 30, 46, 0);
              margin: 5px;
              transition: box-shadow 0.2s ease-in-out, background-color 0.2s ease-in-out;
          }

          button:hover {
              background-color: rgba(49, 50, 68, 0.1);
          }

          button:focus {
              background-color: #cba6f7;
              color: #1e1e2e;
          }

          #lock {
              background-image: image(url("${icon-location}/lock.png"));
          }

          #logout {
              background-image: image(url("${icon-location}/logout.png"));
          }

          #suspend {
              background-image: image(url("${icon-location}/suspend.png"));
          }

          #shutdown {
              background-image: image(url("${icon-location}/shutdown.png"));
          }

          #reboot {
              background-image: image(url("${icon-location}/reboot.png"));
          }
        '';
    };
  };
}
