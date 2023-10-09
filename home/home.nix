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
    ./theming.nix
    ./others.nix
    ./bin/default.nix

    # Editors
    # ./nvim/neovim.nix
    ./emacs/emacs.nix
    # ./vscode/vscode.nix

    # ./kitty/kitty.nix
    ./alacritty/alacritty.nix
    ./mpv/mpv.nix
    ./tmux/tmux.nix
    ./zathura/zathura.nix

    # ./picom/picom.nix

    ./zsh/zsh.nix

    # ./sway/home.nix
    ./hypr/home.nix
    # ./gnome/home.nix
  ];

  # targets.genericLinux.enable = true;

  nixpkgs.config.allowUnfree = true;
  colorScheme = inputs.nix-colors.colorSchemes.dracula;

  home = {
    username = "yvess";
    homeDirectory = "/home/yvess";
    stateVersion = "22.11"; # Please read the comment before changing.

    sessionPath = [ "$HOME/.local/bin" ];
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
