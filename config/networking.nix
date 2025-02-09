{
  config,
  options,
  lib,
  ...
}:
let
  inherit (lib) mkMerge;
in
{
  config = mkMerge [
    {
      networking.hostName = "nixos"; # Define your hostname.
      networking.networkmanager.enable = true;
    }
  ];
}
