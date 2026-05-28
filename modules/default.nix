{
  config,
  inputs,
  ...
}:
# Credit to iynaix
let
  mkNixos =
    host:
    {
      system ? "x86_64-linux",
      user ? "yvess",
      gitUser ? "YvesStraten",
      email ? "yves.straten@gmail.com",
      shell ? "fish",
      nixpkgs ? inputs.nixpkgs,
    }:
    nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = { inherit inputs; };

      modules = [
        config.flake.nixosModules.${host}
        config.flake.nixosModules.core

        {
          config.custom.constants = {
            inherit
              user
              gitUser
              email
              shell
              ;
          };
        }

        (nixpkgs.lib.mkAliasOptionModule
          [ "hm" ]
          [
            "home-manager"
            "users"
            user
          ]
        )

        inputs.home-manager.nixosModules.home-manager
        {
          home-manager = {
            backupFileExtension = "backup";
            useGlobalPkgs = true;
            extraSpecialArgs = { inherit inputs; };
            users.${user} = {
              imports = [
                config.flake.homeModules.core
                config.flake.homeModules.${host}
              ];
            };
          };
        }
      ];
    };
in
{
  flake.nixosConfigurations = {
    deck = mkNixos "deck" { user = "bazzite"; };
    vivobook = mkNixos "vivobook" { };
    wsl = mkNixos "wsl" { nixpkgs = inputs.nixpkgs-stable; };
  };
}
