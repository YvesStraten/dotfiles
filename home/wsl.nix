{ config
, pkgs
, inputs
, gitUser
, user
, email
, ...
}: {
  imports = [
    # Main stuff
    inputs.nix-colors.homeManagerModules.default
    ./dev
    ./bin/default.nix
    ./theming.nix
  ];

  colorScheme = inputs.nix-colors.colorSchemes.dracula;

  home = {
    username = user;
    homeDirectory = "/home/${user}";
    stateVersion = "22.11"; # Please read the comment before changing.

    packages = with pkgs; [ ani-cli openssh ];
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
