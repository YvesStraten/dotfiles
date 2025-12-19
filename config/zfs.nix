{
  config,
  options,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.custom.zfs;
  inherit (lib) mkEnableOption mkIf;
in
{
  options.custom.zfs.enable = mkEnableOption "Use zfs for this system" // {
    default = true;
  };

  config = mkIf cfg.enable {
    services.sanoid = {
      enable = true;
      datasets = {
        "zroot/home" = {
          hourly = 50;
          daily = 15;
          weekly = 3;
          monthly = 1;
        };

        "zroot/libvirt/WIN" = {
          hourly = 4;
        };
      };
    };

    fileSystems = {
      "/" = {
        device = "zroot/root";
        fsType = "zfs";
        neededForBoot = true;
      };

      "/boot" = {
        device = "/dev/disk/by-label/NIXBOOT";
        fsType = "vfat";
      };

      "/nix" = {
        device = "zroot/nix";
        fsType = "zfs";
      };

      "/home" = {
        device = "zroot/home";
        fsType = "zfs";
      };

      "/var" = {
        device = "zroot/var";
        fsType = "zfs";
      };
    };

    swapDevices = [
      {
        device = "/dev/disk/by-label/SWAP";
      }
    ];

    boot = {
      zfs.allowHibernation = true;
      zfs.forceImportRoot = false;
      supportedFilesystems = [
        "zfs"
      ];
    };

    environment.systemPackages = [
      pkgs.sanoid
    ];
  };
}
