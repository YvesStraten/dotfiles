{ lib, ... }:
{
  flake.nixosModules.core =
    {
      config,
      pkgs,
      ...
    }:
    let
      inherit (config.custom.constants) user shell;
      inherit (lib) mkMerge;
    in
    {
      config = mkMerge [
        {
          programs.${shell}.enable = true;
          users = {
            users.${user} = {
              shell = pkgs.${shell};
              isNormalUser = true;
              description = "${user}";
              extraGroups = [
                "users"
                "networkmanager"
                "wheel"
                "audio"
                "libvirtd"
                "docker"
                "gamemode"
                "dialout"
                "fuse"
              ];
            };
          };

          programs = {
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
    };
}
