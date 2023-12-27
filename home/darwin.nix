{ config
, pkgs
, inputs
, ...
}: {
  imports = [
    inputs.nix-colors.homeManagerModules.default
    # Main stuff
    ./languages.nix
    ./utilities.nix
    ./theming-darwin.nix
    ./bin/default.nix

    # Editors
    ./nvim/neovim.nix

    ./alacritty/alacritty.nix
    ./tmux/tmux.nix
    ./firefox/firefox.nix

    ./zsh/zsh.nix
  ];

  # targets.genericLinux.enable = true;

  nixpkgs.config.allowUnfree = true;
  colorScheme = inputs.nix-colors.colorSchemes.gruvbox-light-medium;

  home = {
    username = "yvess";
    homeDirectory = "/Users/yvess";
    stateVersion = "22.11"; # Please read the comment before changing.

    sessionPath = [ "$HOME/.local/bin" ];
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.git = {
    enable = true;
    userName = "YvesStraten";
    userEmail = "yves.straten@gmail.com";
  };
}
