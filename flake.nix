{
  description = "My system";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-old.url = "github:nixos/nixpkgs/nixos-22.11";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager-old = {
        url = "github:nix-community/home-manager/release-22.11";
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
  };

  outputs = {
    self,
    nixpkgs,
    nixpkgs-old,
    home-manager,
    home-manager-old,
    hyprland,
    hyprpicker,
    hypr-contrib,
    nixos-hardware,
    nixos-wsl,
    ...
  } @ inputs: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    nixosConfigurations = {
      nitro = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {inherit inputs;};
        modules = [
          ./hosts/nixos/hardware-configuration.nix
          ./modules/core/bootloader.nix
          ./modules/core/hyprland.nix
          ./modules/core/networking.nix
          ./modules/core/nvidia.nix
          ./modules/core/pkgs.nix
          ./modules/core/security.nix
          ./modules/core/services.nix
          ./modules/core/settings.nix
          ./modules/core/sound.nix
          ./modules/core/time.nix
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

      wsl = nixpkgs-old.lib.nixosSystem {
        inherit system;
        modules = [
          ./hosts/wsl/wsl.nix
          nixos-wsl.nixosModules.wsl 
          home-manager-old.nixosModules.home-manager {
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
    };
  };
}
