_: {
  flake.nixosModules.tailscale =
    {
      pkgs,
      ...
    }:
    {
      environment.systemPackages = [ pkgs.tailscale ];

      services.tailscale = {
        enable = true;
        openFirewall = true;
      };
    };
}
