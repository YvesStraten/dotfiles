{ pkgs, ... }:
{
  custom = {
    fish.enable = true;
    hyprland.enable = true;
    # gnome.enable = true;
    ghostty.enable = true;
    nvim.enable = true;
    vscode.enable = true;
    tmux.enable = true;
    languages.enable = true;
    office.enable = true;
    pass.wayland = true;

    general.extraPackages = with pkgs; [
      spotify
      slack
      peaclock
      discord

      filezilla
      btop
      teams-for-linux

      ani-cli-rofi

      qpwgraph
    ];
  };

}
