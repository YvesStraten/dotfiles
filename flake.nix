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

    systems = {
      url = "github:nix-systems/default";
      flake = false;
    };

    nixpkgs-stable.url = "github:/NixOS/nixpkgs/nixos-25.05";
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs = {
        nixpkgs.follows = "nixpkgs-stable";
        flake-compat.follows = "";
      };
    };
    home-manager-stable = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs-stable";
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    firefox-addons = {
      url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    zathura-dracula = {
      url = "github:dracula/zathura";
      flake = false;
    };

    nvf = {
      url = "github:NotAShelf/nvf";
      inputs = {
        flake-compat.follows = "";
        flake-parts.follows = "flake-parts";
        systems.follows = "systems";
      };
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      # Use the same nixpkgs
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pre-commit-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        gitignore.follows = "gitignore";
        flake-compat.follows = "";
      };
    };

    noctalia = {
      url = "github:noctalia-dev/noctalia-shell";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    import-tree.url = "github:vic/import-tree";

    dolphin-overlay = {
      url = "github:rumboon/dolphin-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    vicinae.url = "github:vicinaehq/vicinae";
  };

  # Add cachix to rebuilds faster
  nixConfig = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    extra-trusted-public-keys = [
      "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "vicinae.cachix.org-1:1kDrfienkGHPYbkpNj1mWTr7Fm1+zcenzgTizIcI3oc="
    ];
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://cache.nixos-cuda.org"
      "https://vicinae.cachix.org"
    ];
  };

  outputs =
    {
      nixpkgs,
      nvf,
      flake-parts,
      pre-commit-hooks,
      self,
      ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.flake-parts.flakeModules.modules
        inputs.home-manager.flakeModules.home-manager
        (inputs.import-tree ./modules)
      ];

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
                shellHook = checks.shellHook + ''
                  export JAVA_HOME="${pkgs.openjdk25.home}"

                '';
                buildInputs = checks.enabledPackages ++ [
                  pkgs.openjdk25
                  pkgs.gradle-packages.gradle_9
                ];
              };

            quickshell =
              let
                quickshell = inputs.dankMaterialShell.packages.${system}.default;
                checks = self.checks.${system}.pre-commit-check;
                shellHook = checks.shellHook + ''
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
          "x86_64-linux" =
            let
              pkgs = nixpkgs.legacyPackages."x86_64-linux";
            in
            { } // (import ./packages { inherit pkgs; });
        };
      };
    };
}
