{
  config,
  pkgs,
  lib,
  ...
}: {
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.yvess = {
    isNormalUser = true;
    description = "Yves Straten";
    extraGroups = ["networkmanager" "wheel" "audio" "libvirtd"];
  };

  programs.zsh.enable = true;
  programs.dconf.enable = true;
  users.defaultUserShell = pkgs.zsh;

  security.pam.services.swaylock = {};

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;
}
