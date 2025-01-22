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

  };

  swapDevices = [
    {
      device = "/dev/disk/by-label/SWAP";
    }
  ];
}
