{
  config,
  pkgs,
  lib,
  ...
}: {
  boot.loader = {
    grub = {
      enable = true;
      efiSupport = true;
      device = "nodev";
    };
    efi.canTouchEfiVariables = true;
    efi.efiSysMountPoint = "/boot/";
  };
  boot.kernelPackages = pkgs.linuxPackages_zen;
}
