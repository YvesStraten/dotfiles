{
  config,
  options,
  pkgs,
  lib,
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
      enable = mkEnableOption "Enable virtualisation";
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
      systemd.services.docker.wantedBy = mkForce [ ];
      virtualisation.docker.enable = true;
      environment.systemPackages = with pkgs; [
        distrobox
        virt-manager
      ];
    })
  ];
}
