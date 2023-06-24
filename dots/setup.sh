#!/bin/sh
# My own rm -rf "$HOME/.local/share/nvim"
rm -rf "$HOME/.config/nvim"
rm -r "$HOME/.config/mpv"
rm -r "$HOME/.config/ranger"
rm -r "$HOME/.config/zathura"
rm -r "$HOME/.config/i3"
rm -r "$HOME/.config/picom"

git clone https://github.com/NvChad/NvChad ~/.config/nvim --depth 1
ln -s "$(pwd)/nvim/custom" ~/.config/nvim/lua/custom

ln -s "$(pwd)/mpv" ~/.config/mpv
ln -s "$(pwd)/ranger" ~/.config/ranger
ln -s "$(pwd)/zathura" ~/.config/zathura
ln -s "$(pwd)/i3" ~/.config/i3
ln -s "$(pwd)/picom" ~/.config/picom

#Other
rm -r "$HOME/.config/dunst"
rm -r "$HOME/.config/hypr"
rm -r "$HOME/.config/kitty"
rm -r "$HOME/.config/pipewire"
rm -r "$HOME/.config/rofi"
rm -r "$HOME/.config/swaylock"
rm -r "$HOME/.config/waybar"
rm -r "$HOME/.config/wlogout"
rm -r "$HOME/.config/tmux"

ln -s "$(pwd)/dunst" ~/.config/dunst
ln -s "$(pwd)/hypr" ~/.config/hypr
ln -s "$(pwd)/kitty" ~/.config/kitty
ln -s "$(pwd)/rofi" ~/.config/rofi
ln -s "$(pwd)/swaylock" ~/.config/swaylock
ln -s "$(pwd)/waybar" ~/.config/waybar
ln -s "$(pwd)/wlogout" ~/.config/wlogout 
ln -s "$(pwd)/tmux" ~/.config/tmux 

#polybar setup
rm -rf "$HOME/.config/polybar/"
git clone --depth=1 https://github.com/adi1090x/polybar-themes.git ~/Git-repos/ 
cd ~/Git-repos/polybar-themes/ 
./setup.sh 
