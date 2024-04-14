{
  config,
  pkgs,
  inputs,
  user,
  ...
}: {
  imports = [
    inputs.nix-colors.homeManagerModules.default
    # Main stuff
    ./utils
    ./theming.nix
    # ./bin

    ./dev

    ./dconf/dconf.nix
    # ./plasma/home.nix
    # ./sway/home.nix
    # ./gnome/home.nix
  ];

  nixpkgs.config.allowUnfree = true;
  colorScheme = inputs.nix-colors.colorSchemes.dracula;

  home = {
    username = user;
    homeDirectory = "/home/${user}";
    stateVersion = "22.11"; # Please read the comment before changing.

    sessionPath = ["$HOME/.local/bin"];
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
