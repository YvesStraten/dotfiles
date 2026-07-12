_: {
  flake.nixosModules.security =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.keepassxc
      ];

      programs.gnupg.agent.enable = true;
    };
}
