{
  pkgs,
  inputs,
  lib,
  ...
}:
{
  imports = [
    ../wlogout/wlogout.nix
    ../dunst/dunst.nix
    ../rofi/rofi.nix
  ];
  
  hm = {
    custom.hyprland.enable = true;
  };
}
