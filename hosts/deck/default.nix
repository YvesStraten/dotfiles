{
  pkgs,
  lib,
  user,
  config,
  ...
}: {
  imports = [
    ./hardware.nix
  ];

  nixpkgs.config.allowUnfree = true;

  jovian = {
    steam = {
      enable = true;
      autoStart = true;
      user = "bazzite";
      desktopSession = "plasma";
    };

    devices.steamdeck.enable = true;
    decky-loader = {
      enable = true;
      # TODO: FIGURE OUT WHY fantastic cannot create in `/tmp/`
      stateDir = "${config.users.users.${user}.home}/.local/share/decky";

      extraPackages = with pkgs; [
        curl
        unzip
        util-linux
        gnugrep
        readline
        procps
        pciutils
        libpulseaudio
      ];
    };
  };

  programs = {
    xwayland.enable = true;
    nix-ld.enable = true;

    steam = {
      enable = true;
      remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
      dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
      localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers

      extraPackages = with pkgs; [
        kdePackages.breeze
      ];
    };
  };

  boot = {
    supportedFilesystems = [
      "ntfs"
      "btrfs"
    ];

    loader = {
      timeout = 0;
      grub = {
        enable = true;
        efiSupport = true;
        useOSProber = true;

        mirroredBoots = [
          {
            devices = ["nodev"];
            path = "/boot";
          }
        ];
      };

      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/";
      };
    };
  };

  custom = {
    zfs.enable = lib.mkForce false;
    power.enable = lib.mkForce false;
    boot.enable = lib.mkForce false;
    bluetooth.enable = lib.mkForce false;
    virtualisation.docker.enable = true;
    # Done by jovian
    sound.enable = true;
  };

  services.printing = {
    enable = true;
    drivers = [
      pkgs.gutenprintBin
      pkgs.gutenprint
      pkgs.cnijfilter2
    ];
  };

  networking.networkmanager.enable = true;
  services = {
    desktopManager.plasma6.enable = true;
    flatpak.enable = true;
  };

  systemd.services.flatpak-repo = {
    wantedBy = ["multi-user.target"];
    path = [pkgs.flatpak];
    script = ''
      flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
    '';
  };

  environment.systemPackages = [pkgs.vesktop pkgs.lutris pkgs.heroic pkgs.steam-rom-manager];
  # Autostart steam in kde
  environment.etc."xdg/autostart/steam.desktop".source = "${pkgs.steam}/share/applications/steam.desktop";

  # Set your time zone.
  time.timeZone = "Asia/Makassar";

  system.stateVersion = "25.05";
}
