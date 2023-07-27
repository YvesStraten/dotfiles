{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    dunst
    kitty
    git
    libappindicator
    showmethekey
    config.nur.repos.ocfox.swww
    brightnessctl
    swaylock-effects
    xfce.thunar

    # rofi-wayland
    rofi-emoji
    btop
    wofi
    inputs.hypr-contrib.packages.${pkgs.system}.grimblast
    inputs.hyprpicker.packages.${pkgs.system}.hyprpicker
    distrobox
    wlogout
    wl-clipboard
    virt-manager
    pamixer
    pavucontrol
    udiskie
    swayidle
    wlsunset
    eww
    neofetch
    appimage-run
    swaynag-battery
    vulkan-validation-layers
    wmenu
    glxinfo
    mesa-demos
    autotiling
    jq

#    nixgl.nixGLNvidia

    dmenu
    i3status
    i3lock-fancy
    xautolock
    xss-lock
    sxhkd
    xorg.xrandr
    arandr
    picom-jonaburg
    maim
    xclip
    feh
    polybar
    sshfs
    feh
    lxappearance
    rofi
  ];

  programs.wshowkeys.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?
}
