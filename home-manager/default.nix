{
  config,
  options,
  inputs,
  user,
  lib,
  ...
}:
let
  inherit (lib) mkMerge mkIf;
in
{
  imports = [
    ./hyprland/hyprland.nix
    ./hyprpanel.nix
    ./nwg-dock.nix
    ./swappy.nix
    ./waybar/waybar.nix
    ./quickshell.nix
    ./udisks.nix
    ./dev
    ./rofi.nix
    ./dunst.nix
    ./pass.nix
    ./kanshi.nix
    ./utils
    ./wlogout/wlogout.nix
    ./i3/i3.nix
    ./picom/picom.nix
    ./polybar/polybar.nix
    ./xdg-dirs.nix
    ./theming.nix
    ./gnome.nix
  ];

  config = mkMerge [
    {
      home = {
        username = user;
        homeDirectory = "/home/${user}";
        stateVersion = "22.11"; # Please read the comment before changing.

        sessionPath = [ "$HOME/.local/bin" ];
      };
    }

    {
      # Let Home Manager install and manage itself.
      programs.home-manager.enable = true;
    }
  ];
}
