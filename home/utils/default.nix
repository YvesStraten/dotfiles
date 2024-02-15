{ pkgs, ... }:

{
  imports = [
    ./general.nix
    ./firefox.nix
    ./alt-tab.nix
    # ./mpv.nix
  ];
}
