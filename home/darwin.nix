{ config
, pkgs
, gitUser
, email
, inputs
, user
, ...
}: {
  imports = [
    inputs.nix-colors.homeManagerModules.default
    # Main stuff
    ./theming-darwin.nix
    ./bin/default.nix

    ./dev
    ./utils
  ];

  nixpkgs.config.allowUnfree = true;
  colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-medium;

  home = {
    username = user;
    homeDirectory = "/Users/${user}";
    stateVersion = "22.11"; # Please read the comment before changing.

    sessionPath = [
      "$HOME/.config/emacs/bin"
      "/Applications/XAMPP/bin"
    ];
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
