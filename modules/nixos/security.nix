{ pkgs, user, shell, ... }: {
  programs.${shell}.enable = true;

  fonts.packages = with pkgs; [ corefonts ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users = {
    defaultUserShell = pkgs.${shell};
    users.${user} = {
      isNormalUser = true;
      description = "${user}";
      extraGroups = [
        "networkmanager"
        "wheel"
        "audio"
        "libvirtd"
        "docker"
        "dialout"
        "fuse"
      ];
    };
  };

  programs.dconf.enable = true;
  services.usbmuxd.enable = true;
  programs.nh = {
    enable = true;
    flake = "/home/${user}/dotfiles";
  };

  security.polkit.enable = true;
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  networking.firewall.enable = false;

  hardware.steam-hardware.enable = true;
}
