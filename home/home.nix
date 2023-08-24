{ config
, pkgs
, ...
}: {
  imports = [
    # Main stuff
    ./languages.nix
    ./utilities.nix
    ./theming.nix
    ./others.nix

    # Editors
    ./nvim/neovim.nix
    ./emacs/emacs.nix

    ./dunst/dunst.nix
    ./kitty/kitty.nix
    ./mpv/mpv.nix
    ./tmux/tmux.nix
    ./zathura/zathura.nix
    ./copyq/copyq.nix

    ./picom/picom.nix

    ./zsh/zsh.nix
    ./hypr/hyprland.nix
    ./swaylock/swaylock.nix
    ./wlogout/wlogout.nix
    ./waybar/waybar.nix
    # ./sway/sway.nix
  ];

  # targets.genericLinux.enable = true;

  home = {
    username = "yvess";
    homeDirectory = "/home/yvess";
    stateVersion = "22.11"; # Please read the comment before changing.

    sessionPath = [
      "$HOME/.local/bin"
    ];

    sessionVariables = {
      TERMINAL = "kitty";
    };
  };

  services = {
    kdeconnect = {
      enable = true;
      indicator = true;
    };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
