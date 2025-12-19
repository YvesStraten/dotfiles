{
  config,
  options,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkMerge;
in
{
  config = mkMerge [
    {
      networking = {
        networkmanager.enable = true;
        networkmanager.plugins = with pkgs; [
          networkmanager-openvpn
        ];
      };
    }
  ];
}
