_: {
  flake.nixosModules.core =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.keepassxc
      ];

      programs.gnupg.agent.enable = true;
    };
}
