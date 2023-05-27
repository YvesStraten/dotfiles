{
  description = "My system";
  inputs = {
   nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
   home-manager = {
	url = "github:nix-community/home-manager";
	inputs.nixpkgs.follows = "nixpkgs";
   };
   hyprland.url = "github:hyprwm/hyprland";
  };

  outputs = { self, nixpkgs, home-manager, hyprland, ...}: 
	let 
		system = "x86_64-linux";
		pkgs = nixpkgs.legacyPackages.${system};
	in {
	nixosConfigurations = {
		nitro = nixpkgs.lib.nixosSystem {
			modules = [
		 		./configuration.nix
				hyprland.nixosModules.default
		];
	  };
	};
	homeConfigurations = {
		yvess = home-manager.lib.homeManagerConfiguration {
			inherit pkgs;
			modules = [
				./dots/home-manager/home.nix
			];
		};
	};
   }; 
}
