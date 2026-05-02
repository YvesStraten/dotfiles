{
  self,

  config,
  ...
}:
{
  flake.nixosModules.vivobook =
    { pkgs, ... }:
    {
      imports = [
        ../../../overlays/default.nix
        self.nixosModules.vivobookHardware
        self.nixosModules.gui
        self.nixosModules.kde
        self.nixosModules.hardware_laptop
        self.nixosModules.virtualisation
        self.nixosModules.zfs
        self.nixosModules.tailscale
        self.nixosModules.hardware_nvidia
      ];

      services = {
        udisks2.enable = true;

        printing = {
          enable = true;
          drivers = [
            pkgs.gutenprintBin
            pkgs.gutenprint
          ];
        };

        flatpak.enable = true;

        gvfs.enable = true;
        samba = {
          enable = true;
          openFirewall = true;
          settings = {
            global = {
              "workgroup" = "WORKGROUP";
              "server string" = "Yvess laptop";
              "security" = "user";
              "guest account" = "nobody";
              "map to guest" = "bad user";
            };
            "home_share" = {
              "path" = "/home/yvess";
              "valid users" = "yvess";
              writeable = "yes";
            };
          };
        };
        avahi = {
          enable = true;
          nssmdns4 = true;
          publish.enable = true;
        };
      };

      boot = {
        kernel.sysctl."vm.swappiness" = 10;
        plymouth = {
          enable = true;
          theme = "bgrt";
          themePackages = with pkgs; [
            # By default we would install all themes
            (adi1090x-plymouth-themes.override {
              selected_themes = [ "rings" ];
            })
          ];
        };

        kernelParams = [
          "zfs.zfs_arc_max=12884901888"
          "preempt=full"
        ];

        supportedFilesystems = [
          "ntfs"
          "btrfs"
          "hpfs"
        ];
        loader = {
          timeout = 4;
          grub = {
            theme = pkgs.yvess.sekiro;
            useOSProber = true;
          };
        };
      };

      hardware = {
        logitech.wireless = {
          enable = true;
          enableGraphical = true;
        };
        xpadneo.enable = true;
        nvidia.prime = {
          offload = {
            enable = true;
            enableOffloadCmd = true;
          };

          intelBusId = "PCI:0:2:0";
          nvidiaBusId = "PCI:1:0:0";
        };
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
            map genLocale [
              "en_US"
              "de_DE"
              "sv_SE"
            ];

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
      networking.hostName = "vivobook";

      fileSystems = {
        "/home/yvess/Emulation" = {
          device = "zroot/home/emulation";
          fsType = "zfs";
          options = [ "x-gvfs-hide" ];
          neededForBoot = false;
        };

        "/home/yvess/Games" = {
          device = "zroot/home/games";
          fsType = "zfs";
          options = [ "x-gvfs-hide" ];
          neededForBoot = false;
        };
      };

      programs = {
        localsend.enable = true;
        nix-ld.enable = true;
        fuse.userAllowOther = true;
        gamemode.enable = true;
        steam = {
          enable = true;
          remotePlay.openFirewall = true;
          localNetworkGameTransfers.openFirewall = true;
        };

        gamescope.enable = true;
      };

      environment.systemPackages = [
        pkgs.mangohud
        pkgs.prismlauncher
        pkgs.kdePackages.kio # needed since 25.11
        pkgs.cifs-utils
        pkgs.kdePackages.kio-fuse # to mount remote filesystems via FUSE
        pkgs.kdePackages.kio-extras # extra protocols support (sftp, fish and more)
        pkgs.kdePackages.dolphin
        pkgs.kdePackages.kdegraphics-thumbnailers
      ];

      # This value determines the NixOS release from which the default
      # settings for stateful data, like file locations and database versions
      # on your system were taken. It‘s perfectly fine and recommended to leave
      # this value at the release version of the first install of this system.
      # Before changing this value read the documentation for this option
      # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
      system.stateVersion = "22.11";
    };

}
