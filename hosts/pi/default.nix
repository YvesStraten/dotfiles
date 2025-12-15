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
  ];

  custom = {
    boot.enable = false;
    zfs.enable = false;
    power.enable = false;
  };

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
  };

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
    openFirewall = true;
    settings = {
      global.security = "user";
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
    openFirewall = true;

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
