{
  config,
  options,
  lib,
  ...
}:
let
  cfg = config.custom.zfs;
  inherit (lib) mkEnableOption mkIf;
in
{
  options.custom.zfs.enable = mkEnableOption "Use zfs for this system" // { default = true; };

  config = mkIf cfg.enable {
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
  };
}
