{
  config,
  pkgs,
  lib,
  ...
}: {
  networking.hostName = "nixos"; # Define your hostname.
  networking.networkmanager.enable = true;
}
