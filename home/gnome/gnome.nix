{pkgs, ...}: let
  defaultExtensions = with pkgs.gnomeExtensions; [
    appindicator
    pano
    forge
    espresso
    dock-from-dash
    gsconnect
  ];
in {
  hm = {
    dconf.settings = {
      "org/gnome/shell" = {
        favorite-apps = [
          "firefox.desktop"
          "org.gnome.Nautilus.desktop"
          "emacsclient.desktop"
          "kitty.desktop"
        ];

        enabled-extensions = builtins.map (extension: "${extension.extensionUuid}") defaultExtensions;
      };

      "org/gnome/desktop/interface" = {
        color-scheme = "prefer-dark";
        clock-show-seconds = true;
      };

      "org/gnome/shell/extensions/user-theme" = {
        name = "Catppuccin-Frappe-Standard-Blue-Dark";
      };
    };
  };
  services.xserver = {
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
  };

  environment.systemPackages = defaultExtensions;
  environment.gnome.excludePackages =
    (with pkgs; [
      gnome-photos
      gnome-tour
    ])
    ++ (with pkgs.gnome; [
      gnome-terminal
      gnome-music
      epiphany
      geary
      tali
      iagno
      hitori
      atomix
    ]);
}
