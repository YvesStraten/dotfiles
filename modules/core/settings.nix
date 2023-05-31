{ config, pkgs, lib, ... }
{

  nix.settings.experimental-features = ["nix-command" "flakes"];
  
  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate = pkg: true;
}
