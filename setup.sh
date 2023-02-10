#!/bin/bash
rm -r "$HOME/.config/nvim"
rm -r "$HOME/.config/alacritty"

ln -s "$(pwd)/nvim"

ln -s "$(pwd)/alacritty"
