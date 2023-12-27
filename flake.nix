{
  description = "My Nix based systems";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/23.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    firefox-darwin.url = "github:bandithedoge/nixpkgs-firefox-darwin";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    hyprland.url = "github:hyprwm/hyprland";
    hyprpicker.url = "github:hyprwm/hyprpicker";
    hypr-contrib.url = "github:hyprwm/contrib";
    nur.url = "github:nix-community/NUR";
    nix-colors.url = "github:misterio77/nix-colors";
    devenv.url = "github:cachix/devenv";
  };

  # Add cachix to rebuilds faster
  nixConfig = {
    experimental-features = [ "nix-command" "flakes" ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
    ];
    extra-substituters = [ "https://devenv.cachix.org" "https://nix-community.cachix.org" ];
  };

  outputs =
    { self
    , nixpkgs
    , nix-colors
    , nix-darwin
    , devenv
    , home-manager
    , hyprland
    , hyprpicker
    , hypr-contrib
    , nixos-hardware
    , nur
    , ...
    } @ inputs:
    let
      forAllSystems = function:
        nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-darwin" ] (system: function nixpkgs.legacyPackages.${system});
      pkgs = nixpkgs.legacyPackages."x86_64-linux";
      pkgs-darwin = nixpkgs.legacyPackages."aarch64-darwin";
    in
    {
      ## TODO: split packages by platform
      packages = forAllSystems (pkgs: import ./packages/default.nix { inherit pkgs; });

      devShells = forAllSystems (pkgs: {
        csharp = devenv.lib.mkShell {
          inherit inputs pkgs;
          modules = [ ({ pkgs, ... }: { languages.dotnet.enable = true; }) ];
        };

        web = devenv.lib.mkShell {
          inherit inputs pkgs;
          modules = [
            ({ pkgs, ... }: {
              languages = {
                typescript.enable = true;
                javascript.enable = true;
              };

              packages = with pkgs; [ sqlite mongosh ];
            })
          ];
        };

        document = devenv.lib.mkShell {
          inherit inputs pkgs;
          modules = [
            ({ pkgs, ... }: {
              languages.texlive.enable = true;
            })
          ];
        };

        c = devenv.lib.mkShell {
          inherit inputs pkgs;
          modules = [
            ({ pkgs, ... }: {
              languages = {
                c.enable = true;
                cplusplus.enable = true;
                rust.enable = true;
              };
            })
          ];
        };
      });

      darwinConfigurations = {
        "shaco" = nix-darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          specialArgs = { inherit inputs; };
          modules = [
            ./modules/darwin/configuration.nix
            ./overlays/default.nix
            home-manager.darwinModules.home-manager
            {
              home-manager = {
                extraSpecialArgs = { inherit inputs; };
                useGlobalPkgs = true;
                useUserPackages = true;
                users.yvess = { ... }: { imports = [ ./home/darwin.nix ]; };
              };
            }
          ];
        };
      };

      nixosConfigurations = {
        nitro = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit inputs; };
          modules = [
            nur.nixosModules.nur
            ./hosts/nixos/hardware-configuration.nix
            ./modules/default.nix
            nixos-hardware.nixosModules.common-pc-laptop-ssd
            nixos-hardware.nixosModules.common-pc-laptop

            home-manager.nixosModules.home-manager
            {
              home-manager = {
                extraSpecialArgs = { inherit inputs; };
                useGlobalPkgs = true;
                useUserPackages = true;
                users.yvess = { ... }: { imports = [ ./home/home.nix ]; };
              };
            }
          ];
        };
      };

      homeConfigurations = {
        akali = home-manager.lib.homeManagerConfiguration {
          extraSpecialArgs = { inherit inputs; };
          inherit pkgs;
          modules = [ ./home/wsl.nix ./overlays/default.nix ];
        };
      };
    };
}
