{
  config,
  options,
  pkgs,
  lib,
  ...
}:
let
  inherit (lib) mkMerge;
in
{
  config = mkMerge [
    {

      boot = {
        plymouth = {
          enable = true;
          theme = "rings";
          themePackages = with pkgs; [
            # By default we would install all themes
            (adi1090x-plymouth-themes.override {
              selected_themes = [ "rings" ];
            })
          ];
        };

        consoleLogLevel = 0;
        initrd.verbose = false;
        kernelParams = [
          "quiet"
          "splash"
          "boot.shell_on_fail"
          "loglevel=3"
          "rd.systemd.show_status=false"
          "rd.udev.log_level=3"
          "udev.log_priority=3"
          "intel_iommu=on"
        ];

        supportedFilesystems = [
          "ntfs"
          "btrfs"
          "hpfs"
          "zfs"
        ];
        loader = {
          timeout = 0;
          grub = {
            enable = true;
            efiSupport = true;
            zfsSupport = true;
            theme = pkgs.yvess.sekiro;
            useOSProber = true;
            splashImage = null;

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
        extraModulePackages = with config.boot.kernelPackages; [
          v4l2loopback
          xpadneo
        ];
        extraModprobeConfig = ''
          options v4l2loopback nr_devices=2 exclusive_caps=1,1 video_nr=0,1 card_label=v4l2lo0,v4l2lo1
        '';
      };
    }
  ];
}
