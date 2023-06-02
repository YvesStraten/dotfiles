{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./zsh.nix
    ./neovim.nix
    ./languages.nix
    ./utilities.nix
  ];

  nix.settings.experimental-features = ["nix-command" "flakes"];
  nix.package = pkgs.nix;
  targets.genericLinux.enable = true;

  home = {
    username = "nixos";
    homeDirectory = "/home/nixos";
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
