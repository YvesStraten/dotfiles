{
  description = "My Nix based systems";
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";

    # Follow unstable
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Follows unstable
    nixpkgs-stable.url = "github:/NixOS/nixpkgs/nixos-23.11";
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs-stable";
    };
    home-manager-stable = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs-stable";
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

    yazi.url = "github:sxyazi/yazi";

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
    extra-substituters =
      [ "https://devenv.cachix.org" "https://nix-community.cachix.org" ];
  };

  outputs = { nixpkgs, nixpkgs-stable, nixos-hardware, nur, nixvim, nixos-wsl
    , home-manager, home-manager-stable, flake-parts, nix-darwin, self, ...
    }@inputs:
    let
      email = "yves.straten@gmail.com";
      gitUser = "YvesStraten";
      user = "yvess";
      shell = "zsh";
    in flake-parts.lib.mkFlake { inherit inputs; } {
      systems =
        [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      perSystem = { system, ... }:
        let
          nvimConf = import ./home/dev/nvim/config;
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ (import ./overlays/vim.nix { inherit inputs; }) ];
          };
          nixvimLib = nixvim.lib.${system};
          nixvim' = nixvim.legacyPackages.${system};
          nvim = nixvim'.makeNixvimWithModule {
            inherit pkgs;
            module = nvimConf;
            # You can use `extraSpecialArgs` to pass additional arguments to your module files
            extraSpecialArgs = {
              # inherit (inputs) foo;
            };
          };
        in {
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
            theme = pkgs.callPackage ./packages/theme.nix { };
          };
        };

      flake = {
        packages = {
          "aarch64-darwin" =
            let pkgs = nixpkgs.legacyPackages."aarch64-darwin";
            in { } // (import ./packages/packages-darwin.nix { inherit pkgs; });
          "x86_64-linux" = let pkgs = nixpkgs.legacyPackages."x86_64-linux";
          in { } // (import ./packages { inherit pkgs; });
        };
        darwinConfigurations = {
          "shaco" = let shell = "zsh";
          in nix-darwin.lib.darwinSystem {
            system = "aarch64-darwin";
            specialArgs = { inherit inputs user shell; };
            modules = [
              ./modules/darwin/configuration.nix
              ./overlays/default.nix
              home-manager.darwinModules.home-manager
              {
                home-manager = {
                  extraSpecialArgs = {
                    inherit inputs self gitUser email user shell;
                  };
                  useGlobalPkgs = true;
                  useUserPackages = true;
                  users.${user} = { ... }: { imports = [ ./home/darwin.nix ]; };
                };
              }
            ];
          };
        };

        nixosConfigurations = {
          nitro = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit inputs user shell; };
            modules = [
              nur.nixosModules.nur
              ./hosts/nixos/hardware-configuration.nix
              ./modules/default.nix
              nixos-hardware.nixosModules.common-pc-laptop-ssd
              nixos-hardware.nixosModules.common-pc-laptop
              (nixpkgs.lib.mkAliasOptionModule [ "hm" ] [
                "home-manager"
                "users"
                user
              ])

              home-manager.nixosModules.home-manager
              {
                home-manager = {
                  extraSpecialArgs = {
                    inherit inputs gitUser email user shell self;
                  };
                  useGlobalPkgs = true;
                  useUserPackages = true;
                  users.${user} = { ... }: { imports = [ ./home/home.nix ]; };
                };
              }
            ];
          };

          wsl = let
            user = "akali";
            shell = "fish";
          in nixpkgs-stable.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit inputs user shell; };
            modules = [
              ./modules/wsl/wsl.nix
              home-manager-stable.nixosModules.home-manager
              nixos-wsl.nixosModules.wsl
              {
                home-manager = {
                  extraSpecialArgs = {
                    inherit inputs self gitUser email user shell;
                  };
                  useGlobalPkgs = true;
                  useUserPackages = true;
                  users.${user} = { ... }: { imports = [ ./home/wsl.nix ]; };
                };
              }
            ];
          };
        };

        homeConfigurations = let
          pkgs = nixpkgs.legacyPackages."x86_64-linux";
          user = "akali";
        in {
          akali = home-manager.lib.homeManagerConfiguration {
            extraSpecialArgs = { inherit inputs gitUser email user; };
            inherit pkgs;
            modules = [ ./home/wsl.nix ./overlays/default.nix ];
          };
        };

        homeManagerModules = (import ./config);
      };
    };
}
