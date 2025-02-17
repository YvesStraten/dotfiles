{
  config,
  options,
  user, 
  lib,
  ...
}:
let
  inherit (lib) mkMerge;
in
{
  imports = [
    ./hyprland/hyprland.nix
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
    ./i3/i3.nix
    ./picom/picom.nix
    ./polybar/polybar.nix
    ./xdg-dirs.nix
    ./theming.nix
  ];

  config = mkMerge [
    {
      nixpkgs.config.allowUnfree = true;
    }

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
