{ pkgs, ... }:
{
  networking.hostId = "14b2792a";
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

  boot.zfs.allowHibernation = true;
  boot.zfs.forceImportRoot = false;
}
