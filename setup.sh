#!/bin/bash
# My own
rm -r "$HOME/.config/nvim"
rm -r "$HOME/.config/alacritty"
rm -r "$HOME/.config/mpv"

ln -s "$(pwd)/nvim" ~/.config/nvim
ln -s "$(pwd)/alacritty" ~/.config/alacritty
ln -s "$(pwd)/mpv" ~/.config/mpv

#Other
rm -r "$HOME/.config/dunst"
rm -r "$HOME/.config/hypr"
rm -r "$HOME/.config/kitty"
rm -r "$HOME/.config/pipewire"
rm -r "$HOME/.config/rofi"
rm -r "$HOME/.config/swaylock"
rm -r "$HOME/.config/waybar"
rm -r "$HOME/.config/wlogout"
rm "$HOME/.swaybg.sh"

ln -s "$(pwd)/submodules/hyprland-titus/dotconfig/dunst" ~/.config/dunst
ln -s "$(pwd)/submodules/hyprland-titus/dotconfig/hypr" ~/.config/hypr
ln -s "$(pwd)/submodules/hyprland-titus/dotconfig/kitty" ~/.config/kitty
ln -s "$(pwd)/submodules/hyprland-titus/dotconfig/pipewire" ~/.config/pipewire 
ln -s "$(pwd)/submodules/hyprland-titus/dotconfig/rofi" ~/.config/rofi
ln -s "$(pwd)/submodules/hyprland-titus/dotconfig/swaylock" ~/.config/swaylock
ln -s "$(pwd)/submodules/hyprland-titus/dotconfig/waybar" ~/.config/waybar
ln -s "$(pwd)/submodules/hyprland-titus/dotconfig/wlogout" ~/.config/wlogout 
cp "$(pwd)/.swaybg.sh" ~/
