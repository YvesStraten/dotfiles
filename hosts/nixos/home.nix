{ pkgs, ... }: {
  custom = {
    fish.enable = true;
    # hyprland.enable = true;
    gnome.enable = true;
    ghostty.enable = true;
    nvim.enable = true;
    tmux.enable = true;
    languages.enable = true;
    office.enable = true;
    theming.enable = true;
    pass.wayland = true;

    general.extraPackages = with pkgs; [
      spotify
      peaclock
      discord

      filezilla
      btop
      nautilus
      teams-for-linux

      ani-cli-rofi

      qpwgraph
    ];
  };
}
