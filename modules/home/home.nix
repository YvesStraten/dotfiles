{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./zsh.nix
    ./neovim.nix
    ./hyprland.nix
    ./languages.nix
    ./utilities.nix
    ./theming.nix
  ];

  nix.settings.experimental-features = ["nix-command" "flakes"];
  targets.genericLinux.enable = true;

  home = {
    username = "yvess";
    homeDirectory = "/home/yvess";
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
