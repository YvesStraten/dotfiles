{
  description = "My system";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    hyprland.url = "github:hyprwm/hyprland";
    hyprpicker.url = "github:hyprwm/hyprpicker";     
    hypr-contrib.url ="github:hyprwm/contrib";
    alejandra = {
      url = "github:kamadorueda/alejandra/3.0.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixd.url = "github:nix-community/nixd";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    hyprland,
    hyprpicker,
    hypr-contrib,
    alejandra,
    nixos-hardware,
    nixd,
    ...
  }@inputs : let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    nixosConfigurations = {
      nitro = nixpkgs.lib.nixosSystem {
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
        ];
      };

      wsl = nixpkgs.lib.nixosSystem {
          modules = [

          ];
        };
    };
    homeConfigurations = {
      yvess = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./modules/home/home.nix
        ];
      };
      nixos = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./modules/home/wsl.nix
        ];
      };
    };
  };
}
