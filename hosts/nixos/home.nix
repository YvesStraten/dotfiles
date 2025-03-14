{ pkgs, ... }:
{
  custom = {
    fish.enable = true;
    hyprland.enable = true;
    kitty.enable = true;
    emacs.enable = true;
    tmux.enable = true;
    languages.enable = true;
    theming.enable = true;

    general.extraPackages = with pkgs; [
      libreoffice
      spotify
      obs-studio

      filezilla
      btop
      nautilus
      teams-for-linux

      ani-cli-rofi

      qpwgraph
    ];
  };
}
