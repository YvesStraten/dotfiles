{
  config,
  pkgs,
  lib,
  ...
}: {
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

  virtualisation.libvirtd.enable = true;
  virtualisation.docker.enable = true;
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
