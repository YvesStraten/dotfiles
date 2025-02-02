{
  config,
  pkgs,
  lib,
  ...
}:
{
  boot.supportedFilesystems = [
    "ntfs"
    "btrfs"
    "hpfs"
    "zfs"
  ];
  boot.loader = {
    grub = {
      enable = true;
      efiSupport = true;
      zfsSupport = true;
      theme = pkgs.yvess.sekiro;
      useOSProber = true;

      mirroredBoots = [
        {
          devices = [ "nodev" ];
          path = "/boot";
        }

      ];
    };

    efi.canTouchEfiVariables = true;
    efi.efiSysMountPoint = "/boot/";
  };
  boot.extraModulePackages = with config.boot.kernelPackages; [
    v4l2loopback
    xpadneo
  ];
  boot.extraModprobeConfig = ''
    options v4l2loopback nr_devices=2 exclusive_caps=1,1 video_nr=0,1 card_label=v4l2lo0,v4l2lo1
  '';
  boot.initrd.availableKernelModules = [ "vmd" "xhci_pci" "nvme" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  hardware.enableRedistributableFirmware = true;
}
