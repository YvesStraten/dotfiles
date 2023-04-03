#!/bin/bash 
if hyprctl monitors | grep -q "Monitor HDMI-A-1"; then 
  echo "HDMI connected"
  hyprctl keyword monitor eDP-1,disable

  echo "Do you want to disable the external display (y/n)"
  read answer

  if [[ $answer == "y" ]]; then 
    hyprctl keyword monitor eDP-1,enable
    hyprctl reload
  fi
else 
  echo "COOL"
fi
