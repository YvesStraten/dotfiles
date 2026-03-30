{
  flake.nixosModules.hardware_networking =
    { pkgs, ... }:
    {
      networking = {
        networkmanager.enable = true;
        networkmanager.plugins = with pkgs; [
          networkmanager-openvpn
        ];
      };
    };
}
