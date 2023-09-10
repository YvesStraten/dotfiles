{pkgs, ...}: {
  programs.tmux = {
    enable = true;
    plugins = with pkgs.tmuxPlugins; [
      yank
      sensible
			catppuccin
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
