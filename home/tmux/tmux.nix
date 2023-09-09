{pkgs, ...}: {
  programs.tmux = {
    enable = true;
    plugins = with pkgs.tmuxPlugins; [
      yank
      sensible
			{
			 	plugin = dracula;
				extraConfig = ''
					set -g @dracula-show-battery false
					set -g @dracula-show-powerline true
					set -g @dracula-refresh-rate 10
				'';
			}
    ];
		
		baseIndex = 1;
		mouse = true;
		keyMode = "vi";
		prefix = "C-Space";
		
		extraConfig = ''
			
bind '"' split-window -v -c "#{pane_current_path}"
bind % split-window -h -c "#{pane_current_path}"
		'';


		tmuxinator.enable = true;
  };
}
