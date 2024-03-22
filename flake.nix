{
  description = "My Nix based systems";
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:/NixOS/nixpkgs/nixos-23.11";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager-stable = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs-stable";
    };
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs-stable";
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

    nixvim.url = "github:nix-community/nixvim";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    vim-snippets = {
      url = "github:YvesStraten/vim-snippets";
      flake = false;
    };

    ouroboros = {
      url = "github:jakemason/ouroboros.nvim";
      flake = false;
    };
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
    { nixpkgs
    , nixpkgs-stable
    , nixos-wsl
    , home-manager-stable
    , nix-colors
    , emacs-overlay
    , nixvim
    , nix-darwin
    , vim-snippets
    , ouroboros
    , flake-parts
    , devenv
    , home-manager
    , hyprland
    , hyprpicker
    , hypr-contrib
    , nixos-hardware
    , nur
    , self
    , ...
    } @ inputs:
    let
      config = import ./home/dev/nvim/config;
    in
    flake-parts.lib.mkFlake { inherit inputs; }
      {
        systems = [
          "x86_64-linux"
          "aarch64-linux"
          "x86_64-darwin"
          "aarch64-darwin"
        ];

        perSystem = { system, ... }:
          let
            pkgs = import nixpkgs {
              inherit system;
              overlays = [
                (import ./overlays/vim.nix { inherit inputs; })
              ];
            };
            nixvimLib = nixvim.lib.${system};
            nixvim' = nixvim.legacyPackages.${system};
            nvim = nixvim'.makeNixvimWithModule {
              inherit pkgs;
              module = config;
              # You can use `extraSpecialArgs` to pass additional arguments to your module files
              extraSpecialArgs = {
                # inherit (inputs) foo;
              };
            };
          in
          {
            checks = {
              # Run `nix flake check .` to verify that your config is not broken
              default = nixvimLib.check.mkTestDerivationFromNvim {
                inherit nvim;
                name = "A nixvim configuration";
              };
            };

            packages = {
              default = nvim;
              nvim = nvim;
            } // (import ./packages {inherit pkgs; });
          };

        flake = {
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
                    extraSpecialArgs = { inherit inputs self; };
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

            wsl = nixpkgs-stable.lib.nixosSystem {
              system = "x86_64-linux";
              specialArgs = { inherit inputs; };
              modules = [
                ./modules/wsl/wsl.nix
                home-manager-stable.nixosModules.home-manager
                nixos-wsl.nixosModules.wsl
                {
                  home-manager = {
                    extraSpecialArgs = { inherit inputs self; };
                    useGlobalPkgs = true;
                    useUserPackages = true;
                    users.akali = { ... }: { imports = [ ./home/wsl.nix ]; };
                  };
                }
              ];
            };
          };

          homeConfigurations =
            let
              pkgs = nixpkgs.legacyPackages."x86_64-linux";
            in
            {
              akali = home-manager.lib.homeManagerConfiguration {
                extraSpecialArgs = { inherit inputs; };
                inherit pkgs;
                modules = [ ./home/wsl.nix ./overlays/default.nix ];
              };
            };
        };
      };
}
