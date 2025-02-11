{
  config,
  options,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.custom.tmux;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.tmux.enable = mkEnableOption "Enable tmux";

  config = mkIf cfg.enable {
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

      extraConfig = ''
        set -g @tmux-gruvbox "dark"

        bind '"' split-window -v -c "#{pane_current_path}"
        bind % split-window -h -c "#{pane_current_path}"
      '';

      tmuxinator.enable = true;
    };
  };
}
