let
  gpuIDs = [
    "10de:1c8d" # Graphics
    "10de:0fb9" # Audio
  ];
in
{ pkgs, lib, config, ... }: {
  options.vfio.enable = with lib;
    mkEnableOption "Configure the machine for VFIO";

  config =
    let cfg = config.vfio;
    in {
      boot = {
        initrd.kernelModules = [
          "vfio_pci"
          "vfio"
          "vfio_iommu_type1"

          "nvidia"
          "nvidia_modeset"
          "nvidia_uvm"
          "nvidia_drm"
        ];

        kernelParams = [
          # enable IOMMU
          "intel_iommu=on"
        ] ++ lib.optional cfg.enable
          # isolate the GPU
          ("vfio-pci.ids=" + lib.concatStringsSep "," gpuIDs);
      };

      virtualisation.spiceUSBRedirection.enable = true;

      systemd.tmpfiles.rules = [
        "f /dev/shm/looking-glass 0660 yvess kvm -"
      ];

      specialisation."VFIO".configuration = {
        system.nixos.tags = [ "with-vfio" ];
        vfio.enable = true;
      };
    };
}
