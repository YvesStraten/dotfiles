{ config
, options
, lib
, pkgs
, ...
}:
let
  inherit (lib) mkMerge;
in
{
  config = mkMerge [
    {
      networking = {
        hostName = "nixos"; # Define your hostname.
        networkmanager.enable = true;
        networkmanager.plugins = with pkgs; [
          networkmanager-openvpn
        ];
      };
    }
  ];
}
