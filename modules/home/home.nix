{
  config,
  pkgs,
  ...
}: {
  imports = [
    # Main stuff
    ./main/languages.nix
    ./main/utilities.nix
    ./main/theming.nix
    ./main/symlinks.nix
    ./main/neovim.nix

    # Variables

    ./variables/zsh.nix
    # ./variables/hyprland.nix
    # ./variables/sway.nix
  ];

  targets.genericLinux.enable = true;

  home = {
    username = "yvess";
    homeDirectory = "/home/yvess";
    stateVersion = "22.11"; # Please read the comment before changing.

    sessionPath = [
      "$HOME/go/bin"
      "$HOME/.local/bin"
    ];

    sessionVariables = {
      EDITOR = "nvim";
      TERMINAL = "kitty";
    };
  };

  
     services = {
    kdeconnect = {
      enable = true;
      indicator = true;
    };
  };
 

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
