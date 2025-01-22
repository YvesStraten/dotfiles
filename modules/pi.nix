{
  pkgs,
  user,
  shell,
  lib,
  config,
  ...
}:
{
  imports = [
    ./nixos/settings.nix
    ./nixos/time.nix
  ];

  boot.supportedFilesystems = lib.mkForce [
    "btrfs"
    "hpfs"
    "ntfs"
    "ext4"
  ];
  sdImage = {
    imageName = "pi-nixos.img";
    compressImage = false;
  };
  boot.kernelPackages = lib.mkForce pkgs.linuxKernel.packages.linux_rpi4;

  nixpkgs.overlays = [
    (final: super: {
      makeModulesClosure = x: super.makeModulesClosure (x // { allowMissing = true; });
    })
  ];

  hardware = {
    enableRedistributableFirmware = true;
    pulseaudio = {
      enable = true;
      support32Bit = true;
    };
  };

  # services.xserver = {
  #   enable = true;
  #   displayManager.lightdm.enable = true;
  #   videoDrivers = [ "fbdev" ];
  # };

  networking.hostName = "PinixOS";
  networking.firewall.enable = false;
  environment.systemPackages = with pkgs; [
    libraspberrypi
    raspberrypi-eeprom
    ani-cli
  ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = [ "noatime" ];
    };

    "/data" = {
      device = "/dev/sda1";
      fsType = "btrfs";
      options = [
        "noatime"
        "nofail"
      ];
    };
  };

  programs.${shell}.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users = {
    defaultUserShell = pkgs.${shell};
    mutableUsers = true;

    users.${user} = {
      isNormalUser = true;
      description = "${user}";
      initialPassword = "password";
      extraGroups = [
        "networkmanager"
        "wheel"
        "audio"
        "dialout"
      ];
      home = "/home/${user}";
    };
  };

  services.xserver.videoDrivers = [ "fbdev" ];

  systemd.services.sshd.wantedBy = lib.mkOverride 40 [ "multi-user.target" ];
  services = {
    jellyfin = {
      enable = true;
      user = "${user}";
      openFirewall = true;
    };

    openssh = {
      enable = true;
      openFirewall = true;
    };
  };

  services.samba = {
    enable = true;
    securityType = "user";
    openFirewall = true;
    settings = {
      jellyfin = {
        path = "/data";
        browseable = "yes";
        writable = "yes";
        "guest ok" = "no";
      };
    };
  };

  networking.firewall.allowedTCPPorts = [
    445
    139
  ];
  networking.firewall.allowedUDPPorts = [
    137
    138
  ];
  networking.networkmanager.enable = true;
  networking.wireless.enable = lib.mkForce false;

  services.samba-wsdd = {
    enable = true;
    openFirewall = true;
  };

  services.avahi = {
    enable = true;
    nssmdns4 = true;

    publish = {
      enable = true;
      addresses = true;
      domain = true;
      hinfo = true;
      userServices = true;
      workstation = true;
    };

    extraServiceFiles = {
      smb = ''
        <?xml version="1.0" standalone='no'?><!--*-nxml-*-->
        <!DOCTYPE service-group SYSTEM "avahi-service.dtd">
        <service-group>
          <name replace-wildcards="yes">%h</name>
          <service>
            <type>_smb._tcp</type>
            <port>445</port>
          </service>
        </service-group>
      '';
    };
  };

  networking.nameservers = [
    "8.8.8.8"
    "8.8.4.4"
  ];
  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluez;
    powerOnBoot = true;
  };

  networking.defaultGateway = {
    address = "192.168.1.1";
    interface = "eth0";
  };

  security.polkit.enable = true;
  networking.interfaces = {
    eth0.ipv4.addresses = [
      {
        address = "192.168.1.72";
        prefixLength = 24;
      }
    ];
  };

  services.udisks2.enable = true;

  system.stateVersion = "23.11";
}
