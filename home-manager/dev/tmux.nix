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
      ];

      baseIndex = 1;
      mouse = true;
      keyMode = "vi";
      sensibleOnTop = true;
      customPaneNavigationAndResize = true;
      escapeTime = 0;

      extraConfig = ''
        # Fix terminal Title display
        set-option -g set-titles on
        set-option -g set-titles-string "#{pane_title}"

        # Status bar
        set-option -g status-justify absolute-centre
        set-option -g status-position top

        bind '"' split-window -v -c "#{pane_current_path}"
        bind % split-window -h -c "#{pane_current_path}"
        bind g popup -E -w 90% -h 90% -d "#{pane_current_path}" -T lazygit lazygit
      '';

      tmuxinator.enable = true;
    };
  };
}
