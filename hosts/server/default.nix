{
  shell,
  pkgs,
  user,
  ...
}:
{
  custom = {
    zfs.enable = false;
    power.enable = false;
  };

  services = {
    openssh.enable = true;
    qemuGuest.enable = true;
    spice-webdavd.enable = true;
    davfs2.settings = {
      globalSection = {
        ask_auth = false;
      };
    };
  };

  environment.systemPackages = with pkgs; [
    vim
    wget
    git
    tmux
  ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/root";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-label/efi";
      fsType = "vfat";
    };

    "/mnt" = {
      device = "http://localhost:9843/";
      fsType = "davfs";
    };
  };
}
