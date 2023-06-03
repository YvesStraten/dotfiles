{
  description = "My system";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland.url = "github:hyprwm/hyprland";
    hyprpicker.url = "github:hyprwm/hyprpicker";
    hyprpaper.url = "github:hyprwm/hyprpaper";
    hypr-contrib.url ="github:hyprwm/contrib";
    alejandra = {
      url = "github:kamadorueda/alejandra/3.0.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @{
    self,
    nixpkgs,
    home-manager,
    hyprland,
    hyprpicker,
    hyprpaper,
    alejandra,
    hypr-contrib,
    ...
  }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    nixosConfigurations = {
      nitro = nixpkgs.lib.nixosSystem {
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
