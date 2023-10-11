{ pkgs, ... }: {
  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
    plugins = [ pkgs.rofi-emoji ];
    theme = "android_notification";
  };

  home.packages = [
    pkgs.wtype
  ];
}
