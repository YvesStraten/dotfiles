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
    ./nwg-dock.nix
    ./swappy.nix
    ./waybar/waybar.nix
    ./udisks.nix
    ./dev
    ./rofi.nix
    ./dunst.nix
    ./pass.nix
    ./kanshi.nix
    ./utils
    ./wlogout/wlogout.nix
    ./xdg-dirs.nix
    ./theming.nix
    ./gnome.nix

    inputs.dankMaterialShell.homeModules.dankMaterialShell.default
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
