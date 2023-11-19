{ config
, pkgs

, ...
}: {
  imports = [
    # Main stuff
   
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
}
