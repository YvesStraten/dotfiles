{ pkgs, lib, ... }: {
  hm.programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
    plugins = [ pkgs.rofi-emoji ];
    terminal = "${lib.getExe pkgs.kitty}";
  };
}
