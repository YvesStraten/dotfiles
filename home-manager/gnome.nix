{
  config,
  options,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.custom.gnome;
  inherit (lib)
    mkEnableOption
    mkIf
    mkMerge
    mkOption
    types
    ;
in
{
  options.custom.gnome = {
    enable = mkEnableOption "Enable gnome";
    extensions = mkOption {
      type = types.listOf types.package;
      default = with pkgs.gnomeExtensions; [
        appindicator
        pano
        forge
        espresso
        dock-from-dash
        gsconnect
        advanced-alttab-window-switcher
        tiling-shell
        color-picker
      ];

      description = ''
        Extra extensions
      '';
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      dconf.settings = {
        "org/gnome/shell" = {
          disable-user-extensions = false;

          favorite-apps = [
            "firefox.desktop"
            "org.gnome.Nautilus.desktop"
            "emacsclient.desktop"
            "kitty.desktop"
          ];

          enabled-extensions = builtins.map (extension: "${extension.extensionUuid}") cfg.extensions;
        };

        "org/gnome/desktop/wm/preferences" = {
          button-layout = "appmenu:minimize,maximize,close";
        };

        "org/gnome/desktop/interface" = {
          color-scheme = "prefer-dark";
          clock-show-seconds = true;
        };

        "org/gnome/shell/extensions/user-theme" = {
          name = "catppuccin-frappe-blue-standard";
        };
      };
    }
  ]);
}
