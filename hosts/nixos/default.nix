{ pkgs
, lib
, config
, ...
}: {
  imports = [
    ../../overlays/default.nix
    ./hardware.nix
  ];

  custom = {
    nvidia.enable = true;
    virtualisation.enable = true;
    fonts.enable = true;
    tailscale.enable = true;
    design.enable = true;
    kde.enable = true;
  };

  services.xserver.videoDrivers = [
    "modesetting"
  ];

  hardware.nvidia.prime = {
    offload = {
      enable = true;
      enableOffloadCmd = true;
    };

    intelBusId = "PCI:0:2:0";
    nvidiaBusId = "PCI:1:0:0";
  };

  i18n =
    let
      defaultLocale = "sv_SE.UTF-8";
    in
    {
      # Select internationalisation properties.
      inherit defaultLocale;
      supportedLocales =
        let
          genLocale = locale: "${locale}.UTF-8/UTF-8";
        in
        map genLocale [ "en_US" "de_DE" "sv_SE" ];

      extraLocaleSettings = {
        LC_ADDRESS = defaultLocale;
        LC_IDENTIFICATION = defaultLocale;
        LC_MEASUREMENT = defaultLocale;
        LC_MONETARY = defaultLocale;
        LC_NAME = defaultLocale;
        LC_NUMERIC = defaultLocale;
        LC_PAPER = defaultLocale;
        LC_TELEPHONE = defaultLocale;
        LC_TIME = defaultLocale;
      };
    };

  networking.hostId = "14b2792a";
  networking.hostName = "nixos";
  services.printing = {
    enable = true;
    drivers = [
      pkgs.gutenprintBin
      pkgs.gutenprint
      pkgs.canon-cups-ufr2
      pkgs.cnijfilter2
    ];
  };

  fileSystems = {
    "/home/yvess/Emulation" = {
      device = "zroot/home/emulation";
      fsType = "zfs";
      options = [ "x-gvfs-hide" ];
    };
  };

  programs.nix-ld.enable = true;
  programs.fuse.userAllowOther = true;
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    localNetworkGameTransfers.openFirewall = true;
  };

  programs.gamescope.enable = true;
  environment.systemPackages = [ pkgs.mangohud pkgs.prismlauncher ];

  services.flatpak.enable = true;
  xdg.portal.enable = true;

  specialisation = {
    hypr.configuration = {
      config.hm.custom = {
        theming = {
          enable = lib.mkForce true;
          qt.enable = lib.mkForce true;
          gtk.enable = lib.mkForce true;
        };
        hyprland.enable = true;
        gnome.enable = lib.mkForce false;
      };
    };

    i3.configuration = {
      config.hm.custom = {
        i3.enable = true;
        gnome.enable = lib.mkForce false;
      };
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?
}
