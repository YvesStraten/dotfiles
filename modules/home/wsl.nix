{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./zsh.nix
    ./neovim.nix
    ./languages.nix
    ./wsl-variables.nix
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

  services = {
    kdeconnect = {
      enable = true;
      indicator = true;
    };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
