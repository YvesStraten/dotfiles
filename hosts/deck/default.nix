{
  pkgs,
  lib,
  user,
  config,
  ...
}:
{
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
      plugins = with pkgs.decky-plugins; [
        protondb_badges
        css_loader
      ];

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
        mangohud
      ];
    };

    gamescope.enable = true;
  };

  boot =
    let
      kernelParams = [
        "spi_amd.speed_dev=1"
        "log_buf_len=4M"
        "amd_iommu=off"
        "amdgpu.lockup_timeout=5000,10000,10000,5000"
        "amdgpu.gttsize=8128"
        "amdgpu.sched_hw_submission=4"
        "audit=0"
        "loglevel=3"
        "video=DP-1:rotate=90"
        "video=eDP-1:rotate=180"
        "quiet"
        "splash"
      ];
    in
    {
      plymouth = {
        enable = true;
        theme = "bgrt";
      };

      kernelParams = lib.mkForce kernelParams;
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
              devices = [ "nodev" ];
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
    # virtualisation.docker.enable = true;
    # Done by jovian
    sound.enable = true;
  };

  # services.printing = {
  #   enable = true;
  #   drivers = [
  #     pkgs.gutenprintBin
  #     pkgs.gutenprint
  #     pkgs.cnijfilter2
  #   ];
  # };

  networking.networkmanager.enable = true;
  services = {
    desktopManager.plasma6.enable = true;
    flatpak.enable = true;
  };

  systemd.services.flatpak-repo = {
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.flatpak ];
    script = ''
      flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
    '';
  };

  environment.systemPackages = with pkgs; [
    discord
    lutris
    heroic
    steam-rom-manager
    mangohud
    mangojuice
    prismlauncher
    r2modman
  ];
  # Autostart steam in kde
  environment.etc."xdg/autostart/steam.desktop".source =
    "${pkgs.steam}/share/applications/steam.desktop";
  hardware.xpadneo.enable = true;

  system.stateVersion = "25.05";
}
