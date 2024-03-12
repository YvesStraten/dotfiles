{
  config,
  pkgs,
  lib,
  ...
}: {
  nix.settings.experimental-features = ["nix-command" "flakes"];

  nix.settings.trusted-users = [
    "root"
    "yvess"
  ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
}
