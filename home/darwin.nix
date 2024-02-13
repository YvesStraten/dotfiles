{ config
, pkgs
, inputs
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
    username = "yvess";
    homeDirectory = "/Users/yvess";
    stateVersion = "22.11"; # Please read the comment before changing.

    sessionPath = [ "$HOME/.config/emacs/bin" ];
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.git = {
    enable = true;
    userName = "YvesStraten";
    userEmail = "yves.straten@gmail.com";
  };
}
