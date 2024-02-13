{ pkgs, ... }:

{
  imports = [
    ./general.nix
    ./firefox.nix
    # ./mpv.nix
  ];
}
