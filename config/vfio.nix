{
  config,
  options,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.custom.vfio;
  inherit (lib)
    mkMerge
    mkEnableOption
    mkIf
    mkOption
    types
    ;
in
{
  options.custom.vfio = {
    enable = mkEnableOption "Enable vfio";
    vfioIds = mkOption {
      type = types.listOf types.str;
      default = [ ];
    };
    platform = mkOption {
      type = types.enum [
        "intel"
        "amd"
      ];
      default = null;
    };
  };

  config = mkIf (cfg.enable && cfg.vfioIds != [ ]) (mkMerge [
    {
      boot =
        let
          platform = cfg.platform;
        in
        {
          kernelModules = [
            "kvm-${platform}"
            "vfio_virqfd"
            "vfio_pci"
            "vfio_iommu_type1"
            "vfio"
          ];
          kernelParams = [
            "${platform}_iommu=on"
            "${platform}_iommu=pt"
            "kvm.ignore_msrs=1"
          ];
          extraModprobeConfig = "options vfio-pci ids=${builtins.concatStringsSep "," cfg.vfioIds}";
        };

      systemd.tmpfiles.rules = [
        "f /dev/shm/looking-glass 0660 yvess qemu-libvirtd -"
      ];
      environment.systemPackages = [
        pkgs.looking-glass-client
      ];

    }

  ]);
}
