{
  inputs,
  user,
  ...
}:
{
  imports = [
    ../config/alt-tab/alt-tab.nix
    # Main stuff
    ./theming-darwin.nix
    ./bin/default.nix
    ./theming-darwin.nix

    ./dev
    ./utils
    ./pass
  ];

  nixpkgs.config.allowUnfree = true;

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
