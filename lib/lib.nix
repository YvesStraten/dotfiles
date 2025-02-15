{
  mkLinuxHost =
    {
      system ? "x86_64-linux",
      user,
      shell,
      email,
      gitUser,
      nixpkgs,
      inputs,
      self,
      extraModules ? [
      ],
    }:
    nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = {
        inherit inputs user shell;
      };

      modules = [
        (nixpkgs.lib.mkAliasOptionModule
          [ "hm" ]
          [
            "home-manager"
            "users"
            user
          ]

          ../modules/nixos

          inputs.home-manager.nixosModules.home-manager
          {
            home-manager = {
              extraSpecialArgs = {
                inherit
                  inputs
                  gitUser
                  email
                  user
                  shell
                  self
                  ;
              };
              useGlobalPkgs = true;
              useUserPackages = true;
              users.${user} =
                { ... }:
                {
                  imports = [ ./home/home.nix ];
                };
            };
          }
        )
      ] ++ extraModules;
    };

  mkDarwinHost =
    {
      system ? "aarch64-darwin",
      user,
      shell,
      email,
      gitUser,
      nix-darwin,
      inputs,
      self,
      extraModules ? [ ],
    }:
    nix-darwin.lib.darwinSystem {
      inherit system;
      specialArgs = { inherit inputs user shell; };
      modules = [
        ../hosts/darwin/configuration.nix
        ../overlays/default.nix

        inputs.home-manager.darwinModules.home-manager
        {
          home-manager = {
            extraSpecialArgs = {
              inherit
                inputs
                self
                gitUser
                email
                user
                shell
                ;
            };
            useGlobalPkgs = true;
            useUserPackages = true;
            users.${user} =
              { ... }:
              {
                imports = [
                  inputs.mac-app-util.homeManagerModules.default
                  ../hosts/darwin/home.nix
                ];
              };
          };
        }

      ] ++ extraModules;
    };
}
