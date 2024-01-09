{ pkgs, ... }: {
  programs.tmux = {
    enable = true;
    plugins = with pkgs.tmuxPlugins; [
      yank
      sensible
      gruvbox
      vim-tmux-navigator
    ];

    baseIndex = 1;
    mouse = true;
    keyMode = "vi";
    prefix = "C-Space";

    extraConfig = ''
      set -g @tmux-gruvbox "dark"

         bind '"' split-window -v -c "#{pane_current_path}"
         bind % split-window -h -c "#{pane_current_path}"
    '';

    tmuxinator.enable = true;
  };
}
