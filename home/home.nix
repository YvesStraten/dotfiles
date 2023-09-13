{
  config,
  pkgs,
  ...
}: {
  imports = [
    # Main stuff
    ./languages.nix
    ./utilities.nix
    ./theming.nix
    ./others.nix

    # Editors
    ./nvim/neovim.nix
    # ./emacs/emacs.nix
    ./vscode/vscode.nix

    ./kitty/kitty.nix
    ./mpv/mpv.nix
    ./tmux/tmux.nix
    ./zathura/zathura.nix

    # ./picom/picom.nix

    ./zsh/zsh.nix

    # ./sway/home.nix
    ./hypr/home.nix
  ];

  # targets.genericLinux.enable = true;

  nixpkgs.config.allowUnfree = true;

  home = {
    username = "yvess";
    homeDirectory = "/home/yvess";
    stateVersion = "22.11"; # Please read the comment before changing.

    sessionPath = [
      "$HOME/.local/bin"
    ];

    sessionVariables = {
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
