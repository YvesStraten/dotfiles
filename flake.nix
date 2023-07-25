{
  description = "My system";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-old.url = "github:nixos/nixpkgs/nixos-22.11";
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
    nixgl.url = "github:guibou/nixGL";
  };

  outputs = {
    self,
    nixpkgs,
    nixgl,
    nixpkgs-old,
    home-manager,
    hyprland,
    hyprpicker,
    hypr-contrib,
    nixos-hardware,
    nixos-wsl,
    nur,
    ...
  } @ inputs: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    overlays = [
      nixgl.overlay
    ];
  in {
    defaultPackage.${system} = home-manager.defaultPackage.${system};
    defaultPackage.x86_64-darwin = home-manager.defaultPackage.x86_64-darwin;
    defaultPackage.aarch64-darwin = home-manager.defaultPackage.aarch64-darwin;

    nixosConfigurations = {
      nitro = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {inherit inputs;};
        modules = [
          nur.nixosModules.nur
          ./hosts/nixos/hardware-configuration.nix
          ./modules/core/default.nix
          hyprland.nixosModules.default
          nixos-hardware.nixosModules.common-pc-laptop-ssd
          nixos-hardware.nixosModules.common-pc-laptop
          nixos-hardware.nixosModules.common-pc-laptop-acpi_call

          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.yvess = import ./modules/home/home.nix;
            };
          }
        ];
      };

      wsl = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          ./modules/core/wsl/wsl.nix
          nixos-wsl.nixosModules.wsl
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.akali = import ./modules/home/wsl.nix;
            };
          }
        ];
      };
    };
    homeConfigurations = {
      akali = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./modules/home/wsl.nix
        ];
      };
      yvess = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./modules/home/home.nix
        ];
      };
    };
  };
}
