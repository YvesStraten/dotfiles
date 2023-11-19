{ config
, pkgs
, inputs
, ...
}: {
  imports = [
    # Main stuff 
    inputs.nix-colors.homeManagerModules.default
    ./alacritty/alacritty.nix
    ./languages.nix
    ./theming.nix
    ./bin/default.nix

    # Editors
    #./nvim/neovim.nix
    ./emacs/emacs.nix
    ./tmux/tmux.nix

    # Variables
    ./zsh/zsh.nix
  ];

  colorScheme = inputs.nix-colors.colorSchemes.dracula;


  home = {
    username = "akali";
    homeDirectory = "/home/akali";
    stateVersion = "22.11"; # Please read the comment before changing.

    sessionPath = [
      "$HOME/.local/bin"
    ];

    packages = with pkgs; [
      ani-cli
    ];
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.git = {
    enable = true;
    userName = "YvesStraten";
    userEmail = "yves.straten@gmail.com";
  };
}
