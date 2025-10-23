{
  description = "My Nix based systems";
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    jovian = {
      url = "github:YvesStraten/Jovian-NixOS?ref=decky-plugins";
      inputs.nixpkgs.follows = "nixpkgs";
    };

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

    mac-app-util.url = "github:hraban/mac-app-util";
    hyprpanel.url = "github:Jas-SinghFSU/HyprPanel";

    # Follows unstable
    nixpkgs-stable.url = "github:/NixOS/nixpkgs/nixos-25.05";
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs-stable";
    };
    home-manager-stable = {
      url = "github:nix-community/home-manager/release-25.05";
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

    zathura-dracula = {
      url = "github:dracula/zathura";
      flake = false;
    };

    nvf.url = "github:NotAShelf/nvf";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
    quickshell = {
      url = "git+https://git.outfoxxed.me/outfoxxed/quickshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  # Add cachix to rebuilds faster
  nixConfig = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
    ];
    extra-substituters = [
      "https://hyprland.cachix.org"
      "https://devenv.cachix.org"
      "https://nix-community.cachix.org"
    ];
  };

  outputs =
    {
      nixpkgs,
      nixpkgs-stable,
      nur,
      nvf,
      nixos-wsl,
      home-manager,
      home-manager-stable,
      nixos-hardware,
      flake-parts,
      nix-darwin,
      mac-app-util,
      pre-commit-hooks,
      self,
      ...
    }@inputs:
    let
      email = "yves.straten@gmail.com";
      gitUser = "YvesStraten";
      user = "yvess";
      shell = "zsh";
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem =
        { system, ... }:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ (import ./overlays/vim.nix { inherit inputs; }) ];
          };

          nvim =
            (nvf.lib.neovimConfiguration {
              inherit pkgs;
              modules = [ ./packages/neovim ];
            }).neovim;
        in
        {
          checks = {
            pre-commit-check = pre-commit-hooks.lib.${system}.run {
              src = ./.;
              hooks = {
                nixfmt-rfc-style.enable = true;
              };
            };
          };

          packages = {
            default = nvim;
            inherit nvim;
          };

          devShells = {
            default =
              let
                checks = self.checks.${system}.pre-commit-check;
              in
              pkgs.mkShell {
                inherit (checks) shellHook;
                buildInputs = checks.enabledPackages;
              };

            quickshell =
              let
                quickshell = inputs.quickshell.packages.${pkgs.system}.default;
                checks = self.checks.${system}.pre-commit-check;
                shellHook =
                  checks.shellHook
                  + ''
                    export QMLLS_BUILD_DIRS=${pkgs.kdePackages.qtdeclarative}/lib/qt-6/qml/:${quickshell}/lib/qt-6/qml/
                    export QML_IMPORT_PATH=$PWD/src
                  '';
              in
              pkgs.mkShell {

                packages = [
                  quickshell
                  pkgs.kdePackages.qtdeclarative
                ];
                inherit shellHook;
              };
          };
        };

      flake = {
        packages = {
          "aarch64-darwin" =
            let
              pkgs = nixpkgs.legacyPackages."aarch64-darwin";
            in
            { } // (import ./packages/packages-darwin.nix { inherit pkgs; });
          "x86_64-linux" =
            let
              pkgs = nixpkgs.legacyPackages."x86_64-linux";
            in
            { } // (import ./packages { inherit pkgs; });
        };

        lib = import ./lib/lib.nix;

        darwinConfigurations = {
          "shaco" = self.lib.mkDarwinHost {
            inherit
              user
              shell
              email
              gitUser
              nix-darwin
              inputs
              self
              ;
          };
        };

        nixosConfigurations = {
          pi =
            let
              user = "xayah";
            in
            nixpkgs.lib.nixosSystem {
              system = "aarch64-linux";
              specialArgs = {
                inherit inputs user shell;
              };
              modules = [
                ./config
                "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64-new-kernel.nix"
                "${nixos-hardware}/raspberry-pi/4"
                ./hosts/pi

                (nixpkgs.lib.mkAliasOptionModule [ "hm" ] [
                  "home-manager"
                  "users"
                  user
                ])

                home-manager.nixosModules.home-manager
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
                        imports = [
                          ./home-manager
                          ./hosts/pi/home.nix
                        ];
                      };
                  };
                }
              ];
            };

          vivobook =
            let
              shell = "fish";
            in
            nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              specialArgs = {
                inherit
                  inputs
                  user
                  shell
                  self
                  ;
              };
              modules = [
                nur.modules.nixos.default
                ./hosts/nixos
                ./config/default.nix
                (nixpkgs.lib.mkAliasOptionModule [ "hm" ] [
                  "home-manager"
                  "users"
                  user
                ])

                home-manager.nixosModules.home-manager
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
                    backupFileExtension = "backup";
                    useGlobalPkgs = true;
                    users.${user} =
                      { ... }:
                      {
                        imports = [
                          ./hosts/nixos/home.nix
                          ./home-manager
                        ];
                      };
                  };
                }
              ];
            };

          server =
            let
              user = "utm";
              shell = "fish";
            in
            nixpkgs.lib.nixosSystem {
              system = "aarch64-linux";
              specialArgs = {
                inherit inputs user shell;
              };

              modules = [
                ./config
                ./hosts/server

                (nixpkgs.lib.mkAliasOptionModule [ "hm" ] [
                  "home-manager"
                  "users"
                  user
                ])

                home-manager.nixosModules.home-manager
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
                    users.${user} =
                      { ... }:
                      {
                        imports = [
                          ./hosts/server/home.nix
                          ./home-manager
                        ];
                      };
                  };
                }
              ];
            };

          deck =
            let
              user = "bazzite";
              shell = "fish";
            in
            nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              specialArgs = {
                inherit inputs user shell;
              };

              modules = [
                ./hosts/deck
                inputs.jovian.nixosModules.default
                nur.modules.nixos.default
                ./config

                (nixpkgs.lib.mkAliasOptionModule [ "hm" ] [
                  "home-manager"
                  "users"
                  user
                ])

                home-manager.nixosModules.home-manager
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
                    users.${user} =
                      { ... }:
                      {
                        imports = [
                          ./hosts/deck/home.nix
                          ./home-manager
                        ];
                      };
                  };
                }
              ];
            };

          wsl =
            let
              user = "akali";
              shell = "fish";
            in
            nixpkgs-stable.lib.nixosSystem {
              system = "x86_64-linux";
              specialArgs = {
                inherit inputs user shell;
              };
              modules = [
                ./config
                ./hosts/wsl

                home-manager-stable.nixosModules.home-manager
                (nixpkgs.lib.mkAliasOptionModule [ "hm" ] [
                  "home-manager"
                  "users"
                  user
                ])

                nixos-wsl.nixosModules.wsl
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
                          ./home-manager
                          ./hosts/wsl/home.nix
                        ];
                      };
                  };
                }
              ];
            };
        };

        homeConfigurations =
          let
            pkgs = nixpkgs.legacyPackages."x86_64-linux";
            user = "bazzite";
            shell = "fish";
          in
          {
            bazzite = home-manager.lib.homeManagerConfiguration {
              extraSpecialArgs = {
                inherit
                  inputs
                  shell
                  gitUser
                  email
                  user
                  self
                  ;
              };
              inherit pkgs;
              modules = [
                ./home/home.nix
                ./overlays/default.nix
              ];
            };
          };

        homeManagerModules = import ./config;
      };
    };
}
