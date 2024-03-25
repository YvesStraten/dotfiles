{ pkgs, ... }:

{
  imports = [
    ./general.nix
    ./firefox.nix
    ./alt-tab.nix
    ./yazi/yazi.nix
    # ./mpv.nix
  ];
}
