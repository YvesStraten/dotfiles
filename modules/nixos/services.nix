{
  config,
  pkgs,
  lib,
  ...
}:
{
  services.upower = {
    enable = true;
    criticalPowerAction = "Hibernate";
  };

  services.gnome.gnome-keyring.enable = true;

  services.logind.extraConfig = ''
    HandlePowerKey=ignore
  '';

  services.printing = {
    enable = true;
    drivers = [
      pkgs.gutenprintBin
      pkgs.gutenprint
    ];
  };

  services.gvfs.enable = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.udisks2.enable = true;

  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      package = pkgs.qemu_kvm;
      runAsRoot = true;
      swtpm.enable = true;

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

  virtualisation.docker.enable = true;
  services.flatpak.enable = true;
  services.thermald.enable = true;
  services.tlp.enable = true;
  services.samba = {
    enable = true;
    openFirewall = true;
  };

  # To make SMB mounting easier on the command line
  environment.systemPackages = with pkgs; [
    cifs-utils
  ];

  services.avahi = {
    enable = true;
    nssmdns4 = true;
    publish.enable = true;
  };
}
