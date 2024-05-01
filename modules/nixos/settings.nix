{
  inputs,
  user,
  ...
}: {
  nix.settings.experimental-features = ["nix-command" "flakes"];

  nix.nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];

  nix.settings.trusted-users = [
    "root"
    user
  ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
}
