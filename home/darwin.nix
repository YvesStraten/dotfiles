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
    # ./nvim/neovim.nix
    ./emacs/emacs.nix

    # ./alacritty/alacritty.nix
    ./kitty/kitty.nix
    ./tmux/tmux.nix
    ./firefox/firefox.nix
    ./alt-tab/alt-tab.nix

    ./zsh/zsh.nix
  ];

  # targets.genericLinux.enable = true;

  # programs.nvchad = {
  #   enable = true;
  # customConfig = ./custom;
  # defaultEditor = true;
  # };

  nixpkgs.config.allowUnfree = true;
  colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-medium;

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
