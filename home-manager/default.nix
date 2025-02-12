{ ... }: {
  imports = [
    ./hyprland/hyprland.nix
    ./swappy.nix
    ./waybar/waybar.nix
    ./udisks.nix
    ./dev
    ./rofi.nix
    ./dunst.nix
    ./pass.nix
    ./kanshi.nix
    ./utils
    ./wlogout/wlogout.nix
  ];
}
