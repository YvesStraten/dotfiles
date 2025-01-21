{ pkgs, ... }:
{
  programs.kitty = {
    enable = true;
    font = {
      name = "FiraCode";
      size = 23;
    };

    shellIntegration = {
      enableZshIntegration = true;
      enableFishIntegration = true;
    };
    extraConfig =
      ''
        macos_quit_when_last_window_closed yes
        titlebar-only yes
        confirm_os_window_close 0
        background_opacity 0.8

        enable_audio_bell no
        include ~/.cache/wal/colors-kitty.conf
      ''
      + (if pkgs.stdenv.isDarwin then "hide_window_decorations no
" else "");
  };
}
