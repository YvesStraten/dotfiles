{
  config,
  lib,
  shell,
  pkgs,
  user,
  ...
}:
let
  inherit (lib) mkMerge;
in
{
  config = mkMerge [
    {
      fonts.packages = with pkgs; [ corefonts ];

      # Define a user account. Don't forget to set a password with ‘passwd’.
      users = {
        defaultUserShell = pkgs.${shell};
        users.${user} = {
          isNormalUser = true;
          description = "${user}";
          extraGroups = [
            "users"
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

      programs = {
        ${shell}.enable = true;
        nh = {
          enable = true;
          flake = "/home/${user}/dotfiles";
        };
      };

      services.usbmuxd.enable = true;

      # Some programs need SUID wrappers, can be configured further or are
      # started in user sessions.
      # programs.mtr.enable = true;

      networking.firewall.enable = false;

      hardware.steam-hardware.enable = true;
    }
  ];
}
