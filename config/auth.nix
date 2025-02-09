{ config, lib, ... }:
let
  inherit (lib) mkMerge;
in
{
  config = mkMerge [
    {
      programs = {
        dconf.enable = true;
        gnupg.agent = {
          enable = true;
          enableSSHSupport = true;
        };
      };
    }

    {
      security = {
        polkit.enable = true;
      };
    }

    {
      services = {
        gnome.gnome-keyring.enable = true;
        # Enable the OpenSSH daemon.
        openssh.enable = true;
      };
    }
  ];
}
