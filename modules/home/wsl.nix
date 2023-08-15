{
  config,
  pkgs,
  ...
}: {
  imports = [
    # Main stuff
    ./main/neovim.nix
    ./main/emacs-wsl.nix
    ./main/symlinks.nix
    ./main/languages.nix
    ./main/theming.nix

    # Variables
    ./variables/zsh.nix
    ./variables/wsl-variables.nix
  ];

  targets.genericLinux.enable = true;

  home = {
    username = "akali";
    homeDirectory = "/home/akali";
    stateVersion = "22.11"; # Please read the comment before changing.

    sessionPath = [
      "$HOME/go/bin"
      "$HOME/.local/bin"
      "$HOME/.config/emacs/bin"
    ];
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
