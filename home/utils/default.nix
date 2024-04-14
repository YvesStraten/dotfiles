{ pkgs, ... }:

{
  imports = [
    ./general.nix
    ./firefox.nix
    ./yazi/yazi.nix
    ./mpv.nix
    ./zathura.nix
    ./thunderbird.nix
  ];
}
