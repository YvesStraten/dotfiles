{
  config,
  options,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.custom.virtualisation;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.virtualisation.enable = mkEnableOption "Enable virtualisation";

  config = mkIf cfg.enable {
    virtualisation = {
      libvirtd = {
        enable = true;
        qemu = {
          package = pkgs.qemu_kvm;
          runAsRoot = true;
          swtpm.enable = true;

          vhostUserPackages = [ pkgs.virtiofsd ];

          ovmf = {
            enable = true;
            packages = [
              (pkgs.OVMF.override {
                secureBoot = true;
                tpmSupport = true;
              }).fd
            ];
          };
        };
      };

      docker.enable = true;
    };

    environment.systemPackages = with pkgs; [
      distrobox
      virt-manager
    ];
  };
}
