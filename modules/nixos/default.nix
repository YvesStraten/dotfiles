{ pkgs, ... }:
{
  imports = [
    ../../overlays/default.nix
    ./hardware.nix

    # Desktops
    ../../home/hypr/hyprland.nix
    # ../home/i3/i3.nix
    # ../home/plasma/plasma.nix
    # ../home/gnome/gnome.nix
  ];

  custom = {
    nvidia.enable = true;
    virtualisation.enable = true;
    
  };
  # Set your time zone.
  time.timeZone = "Asia/Makassar";

  # Select internationalisation properties.
  i18n.defaultLocale = "de_DE.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "de_DE.UTF-8";
    LC_IDENTIFICATION = "de_DE.UTF-8";
    LC_MEASUREMENT = "de_DE.UTF-8";
    LC_MONETARY = "de_DE.UTF-8";
    LC_NAME = "de_DE.UTF-8";
    LC_NUMERIC = "de_DE.UTF-8";
    LC_PAPER = "de_DE.UTF-8";
    LC_TELEPHONE = "de_DE.UTF-8";
    LC_TIME = "de_DE.UTF-8";
  };

  networking.hostId = "14b2792a";
  services.printing = {
    enable = true;
    drivers = [
      pkgs.gutenprintBin
      pkgs.gutenprint
      pkgs.canon-cups-ufr2
      pkgs.cnijfilter2
    ];
  };

  services.flatpak.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?
}
