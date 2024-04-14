{
  config,
  pkgs,
  lib,
  ...
}: {
  boot.supportedFilesystems = [ "ntfs" ];
  boot.loader = {
    grub = {
      enable = true;
      efiSupport = true;
      device = "nodev";
      theme = pkgs.yvess.sekiro;
      useOSProber = true;
    };
    efi.canTouchEfiVariables = true;
    efi.efiSysMountPoint = "/boot/";
  };
  boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.extraModulePackages = with config.boot.kernelPackages; [v4l2loopback xpadneo];
  boot.extraModprobeConfig = ''
    options v4l2loopback nr_devices=2 exclusive_caps=1,1 video_nr=0,1 card_label=v4l2lo0,v4l2lo1
  '';
  boot.kernelParams = [ "snd_sof_intel_hda_common.hda_model=alc255-acer" ];
}
