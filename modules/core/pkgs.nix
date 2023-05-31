{ config, pkgs, lib, ... } : {
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    dunst
    kitty
    git
    libappindicator
    brightnessctl
    swaylock-effects
    rofi-wayland
    btop
    wofi
    wlogout
    grim
    slurp
    wl-clipboard
    pamixer
    pavucontrol
    swaybg
    udiskie
    swayidle
    wlsunset
    neofetch
    hyprpaper
    hyprpicker
    alejandra
    appimage-run
  ];

    # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

}
