{
  config,
  pkgs,
  lib,
  user,
  ...
}: {
  nix.settings.experimental-features = ["nix-command" "flakes"];

  nix.settings.trusted-users = [
    "root"
    user
  ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
}
