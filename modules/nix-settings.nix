{ inputs, lib, ... }:
{
  flake.nixosModules.core =
    {
      config,
      pkgs,
      ...
    }:
    let
      inherit (config.custom.constants) user;
      inherit (lib) mkMerge;
    in
    {
      config = mkMerge [
        {
          nix = {
            package = pkgs.lixPackageSets.stable.lix;
            settings.experimental-features = [
              "nix-command"
              "flakes"
            ];
            nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];

            settings.trusted-users = [
              "root"
              user
            ];

            # Allow unfree packages
          };

          nixpkgs.config.allowUnfree = true;
        }
      ];
    };
}
