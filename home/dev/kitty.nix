{ pkgs, ... }: {
  programs.kitty = {
    enable = true;
    font.name = "JetBrainsMono Nerd Font";
    font.size = 23;
    shellIntegration.enableZshIntegration = true;
    extraConfig = ''
      macos_quit_when_last_window_closed yes
      hide_window_decorations yes
      titlebar-only yes
      confirm_os_window_close 0
      background_opacity 0.8

      enable_audio_bell no
    '';
    theme = "kanagawabones";
  };
}
