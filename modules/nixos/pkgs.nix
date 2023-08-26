{ config
, pkgs
, lib
, inputs
, ...
}: {
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    gh
    xdg-user-dirs
    git
    libappindicator
    showmethekey
    brightnessctl
    btop
    distrobox
    virt-manager
    neofetch
    appimage-run
    mesa-demos
    jq
    sshfs
    pamixer
    pavucontrol
    #    nixgl.nixGLNvidia

    # x11 stuff
    # dmenu
    # i3status
    # i3lock-fancy
    # xautolock
    # xss-lock
    # xorg.xrandr
    # arandr
    # maim
    # xclip
    # feh
    # polybar
    # autotiling
    # eww
    # feh
    # lxappearance

    # Wayland stuff
    inputs.hypr-contrib.packages.${pkgs.system}.grimblast
    inputs.hyprpicker.packages.${pkgs.system}.hyprpicker
    swww
    wl-clipboard
    swayidle
    wlsunset
    wdisplays
    networkmanagerapplet
    polkit_gnome
    vulkan-validation-layers
    swaynag-battery

    # For plasma
    libsForQt5.plasma-browser-integration
    # latte-dock
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
