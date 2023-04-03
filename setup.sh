#!/bin/bash
# My own
rm -r "$HOME/.config/nvim"
rm -r "$HOME/.config/alacritty"
rm -r "$HOME/.config/mpv"
rm -r "$HOME/.config/ranger"

ln -s "$(pwd)/nvim" ~/.config/nvim
ln -s "$(pwd)/alacritty" ~/.config/alacritty
ln -s "$(pwd)/mpv" ~/.config/mpv
ln -s "$(pwd)/ranger" ~/.config/ranger

#Other
rm -r "$HOME/.config/dunst"
rm -r "$HOME/.config/hypr"
rm -r "$HOME/.config/kitty"
rm -r "$HOME/.config/pipewire"
rm -r "$HOME/.config/rofi"
rm -r "$HOME/.config/swaylock"
rm -r "$HOME/.config/waybar"
rm -r "$HOME/.config/wlogout"
sudo rm -r "/opt/bg/"

ln -s "$(pwd)/dunst" ~/.config/dunst
ln -s "$(pwd)/hypr" ~/.config/hypr
ln -s "$(pwd)/kitty" ~/.config/kitty
ln -s "$(pwd)/pipewire" ~/.config/pipewire 
ln -s "$(pwd)/rofi" ~/.config/rofi
ln -s "$(pwd)/swaylock" ~/.config/swaylock
ln -s "$(pwd)/waybar" ~/.config/waybar
ln -s "$(pwd)/wlogout" ~/.config/wlogout 

mkdir /opt/bg/
sudo chgrp -R homeusers /opt/bg/
sudo chmod g+rwx /opt/bg/
cp "$(pwd)/.swaybg.sh" /opt/bg/
sudo cp "$(pwd)/swaybg.desktop" /usr/share/applications/
