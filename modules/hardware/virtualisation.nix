{ lib, ... }:
{
  flake.nixosModules.virtualisation =
    {
      config,
      pkgs,
      ...
    }:
    let
      cfg = config.custom.virtualisation;
      inherit (lib)
        mkMerge
        mkEnableOption
        mkIf
        mkForce
        ;
    in
    {
      options = {
        custom.virtualisation = {
          enable = mkEnableOption "Enable virtualisation" // {
            default = true;
          };
          libvirt.enable = mkEnableOption "Enable libvirt" // {
            default = cfg.enable;
          };
          docker.enable = mkEnableOption "Enable libvirt" // {
            default = cfg.enable;
          };
        };
      };

      config = mkMerge [
        (mkIf cfg.libvirt.enable {
          virtualisation = {
            libvirtd = mkIf cfg.libvirt.enable {
              enable = true;
              qemu = {
                package = pkgs.qemu_kvm;
                runAsRoot = true;
                swtpm.enable = true;

                vhostUserPackages = [ pkgs.virtiofsd ];
              };
            };
          };

          environment.systemPackages = with pkgs; [
            virt-manager
          ];
        })

        (mkIf cfg.docker.enable {
          virtualisation = {
            docker = {
              enable = true;
              enableOnBoot = false;
              storageDriver = mkIf config.custom.zfs.enable "zfs";

              # This is to prevent conflicts with eduroam
              daemon.settings = {
                bip = "10.0.64.1/24";
                default-address-pools = [
                  {
                    base = "10.0.64.0/18";
                    size = 24;
                  }
                ];
              };
            };
          };
          environment.systemPackages = with pkgs; [
            distrobox
            virt-manager
          ];
        })
      ];
    };
}
