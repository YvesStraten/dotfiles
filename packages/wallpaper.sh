#!/usr/bin/env bash
set -euxo pipefail

images=$(find "$(xdg-user-dir "PICTURES")/Wallpapers" -type f) 

selection=$(echo "$images" | rofi -dmenu)
swww img "$selection"

query=$(swww query)

IFS=':'
read -ra newarr <<< "$query"

length="${#newarr}"
picture_string="${newarr[$length - 1]}"
picture="${picture_string:1}"
path="$HOME/.cache"
ext=".${picture##*.}"

mkdir -p "$path/"
temp=$(mktemp -d)

convert -blur 0x25 "$picture" "$temp/lock-image$ext" 
mv "$temp/lock-image$ext" "$path/lock-image"
