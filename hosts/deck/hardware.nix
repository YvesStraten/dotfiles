{ ... }:
let
  mkBtrfsSubVol =
    {
      subVol,
      extraOpts ? [ ],
      label ? "NIXOS",
    }:
    {
      options = [
        "compress=zstd"
        "subvol=${subVol}"
      ] ++ extraOpts;
      label = label;
      fsType = "btrfs";
    };
in
{
  boot.initrd.availableKernelModules = [
    "nvme"
    "xhci_pci"
    "usbhid"
    "sdhci_pci"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];

  fileSystems = {
    "/boot" = {
      label = "NIXBOOT";
      fsType = "vfat";
    };

    "/" = (mkBtrfsSubVol { subVol = "root"; });
    "/home" = (mkBtrfsSubVol { subVol = "home"; });
    "/var" = (mkBtrfsSubVol { subVol = "var"; });
    "/nix" = (
      mkBtrfsSubVol {
        subVol = "nix";
        extraOpts = [ "noatime" ];
      }
    );
  };

  swapDevices = [
    {
      device = "/dev/disk/by-label/SWAP";
    }
  ];
}
