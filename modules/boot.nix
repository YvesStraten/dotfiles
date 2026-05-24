{ lib, ... }:
{
  flake.nixosModules.core = _: {
    boot = lib.mkDefault {
      initrd.systemd.enable = true;
      loader = {
        grub = {
          enable = true;
          efiSupport = true;

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
    };

    systemd.services.NetworkManager-wait-online.wantedBy = lib.mkForce [ ];
  };
}
