{ config, pkgs, lib, user, shell, ... }: {
  programs.${shell}.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users = {
    defaultUserShell = pkgs.${shell};
    users.${user} = {
      isNormalUser = true;
      description = "${user}";
      extraGroups =
        [ "networkmanager" "wheel" "audio" "libvirtd" "docker" "dialout" "fuse" ];
    };
  };

  programs.dconf.enable = true;
  services.usbmuxd.enable = true;
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
  programs.nh = {
    enable = true;
    flake = "/home/${user}/dotfiles";
  };

  security.pam.services.swaylock.text = "auth include login";
  security.polkit.enable = true;
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  hardware.steam-hardware.enable = true;
  hardware.cpu.x86.msr.enable = true;

  # TODO: make this more robust
  systemd.services.bdprochot = let
    script = pkgs.writeShellScriptBin "bdprochot" ''
      ${pkgs.msr-tools}/bin/rdmsr -a -d 0x1FC
      ${pkgs.msr-tools}/bin/wrmsr 0x1FC 2359386
    '';
    toRun = "${lib.getExe script}";
  in {
    wantedBy = [ "multi-user.target" ];

    description = "Disabled bd-prochot";

    serviceConfig = {
      Type = "simple";
      ExecStart = toRun;
    };
  };
}
