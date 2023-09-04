{pkgs, ...}: {
  programs.kitty = {
    enable = true;
    shellIntegration = {
      enableBashIntegration = true;
      enableZshIntegration = true;
    };
    theme = "Dracula";
    font = {
      name = "JetBrainsMono NF";
      size = 15.0;
    };
    keybindings = {
      "super+v" = "paste_from_clipboard";
      "super+c" = "copy_to_clipboard";

      "ctrl+=" = "increase_font_size";
      "ctrl+minus" = "decrease_font_size";
    };
    settings = {
      hide_window_decoratios = false;
      macos_option_as_alt = false;
      enable_audio_bell = false;
      confirm_os_window_close = 0;
    };
  };
}
