{
  description = "My system";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-old.url = "github:nixos/nixpkgs/nixos-23.05";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    hyprland.url = "github:hyprwm/hyprland";
    hyprpicker.url = "github:hyprwm/hyprpicker";
    hypr-contrib.url = "github:hyprwm/contrib";
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs-old";
    };
    nur.url = "github:nix-community/NUR";
    nix-colors.url = "github:misterio77/nix-colors";
    devenv.url = "github:cachix/devenv";
  };

  nixConfig = {
    extra-trusted-public-keys = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };

  outputs =
    { self
    , nixpkgs
    , nix-colors
    , nixpkgs-old
    , devenv
    , home-manager
    , hyprland
    , hyprpicker
    , hypr-contrib
    , nixos-hardware
    , nixos-wsl
    , nur
    , ...
    } @ inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      packages.${system} = import ./packages/default.nix { inherit pkgs; };

      formatter.${system} = pkgs.alejandra;

      devShells.${system} = {
        csharp = devenv.lib.mkShell {
          inherit inputs pkgs;
          modules = [
            ({ pkgs, ... }: {
              languages.dotnet.enable = true;
            })
          ];
        };

        web = devenv.lib.mkShell {
          inherit inputs pkgs;
          modules = [
            ({ pkgs, ... }: {
              languages = {
                typescript.enable = true;
                javascript.enable = true;
              };

              packages = with pkgs; [
                sqlite
                mongosh
              ];
            })
          ];
        };

        c = devenv.lib.mkShell {
          inherit inputs pkgs;
          modules = [
            ({ pkgs, ... }: {
              languages =
                {
                  c.enable = true;
                  cplusplus.enable = true;
                  rust.enable = true;
                };
            })
          ];
        };

        arduino = devenv.lib.mkShell {
          inherit inputs pkgs;
          modules = [
            ({ pkgs, ... }: {
              packages = with pkgs; [
                arduino
                arduino-cli
              ];
            })
          ];
        };
      };

      nixosConfigurations = {
        nitro = nixpkgs.lib.nixosSystem {
          inherit system;
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
                users.yvess = { ... }: {
                  imports = [
                    ./home/home.nix
                  ];
                };
              };
            }
          ];
        };

        Iso = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs; };
          modules = [
            "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
            nur.nixosModules.nur
            ./modules/iso.nix
          ];
        };

        wsl = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs; };
          modules = [
            ./modules/wsl/wsl.nix
            nixos-wsl.nixosModules.wsl
            nur.nixosModules.nur
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                extraSpecialArgs = { inherit inputs; };
                useGlobalPkgs = true;
                useUserPackages = true;
                users.akali = { ... }: {
                  imports = [
                    ./home/wsl.nix
                  ];
                };
              };
            }
          ];
        };
      };

      homeConfigurations = {
        akali = home-manager.lib.homeManagerConfiguration {
          extraSpecialArgs = { inherit inputs; };
          inherit pkgs;
          modules = [
            ./home/wsl.nix
          ];
        };
        yvess = home-manager.lib.homeManagerConfiguration {
          extraSpecialArgs = { inherit inputs; };
          inherit pkgs;
          modules = [
            ./home/home.nix
          ];
        };
      };
    };
}
