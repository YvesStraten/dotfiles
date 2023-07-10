{
  config,
  pkgs,
  ...
}: {
  imports = [
  # Main stuff
    ./main/neovim.nix
    ./main/languages.nix

  # Variables
    ./variables/zsh.nix
    ./variables/wsl-variables.nix
  ];

  nix.settings.experimental-features = ["nix-command" "flakes"];
  targets.genericLinux.enable = true;

  home = {
    username = "akali";
    homeDirectory = "/home/akali";
    stateVersion = "22.11"; # Please read the comment before changing.

    sessionPath = [
      "$HOME/go/bin"
      "$HOME/.local/bin"
    ];
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
