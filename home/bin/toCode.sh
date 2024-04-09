#!/usr/bin/env sh

fileName="$1"
extension="$2"
saveFolder="$HOME/Downloads/$(basename "$fileName" "$extension")png"

pygmentize "$fileName" -o "$saveFolder"
