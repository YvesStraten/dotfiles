{ pkgs, ... }: {
  users.users.yvess = {
    name = "yvess";
    home = "/Users/yvess";
  };

  # nix.settings.sandbox = true;
  nix.settings.trusted-users = [
    "root"
    "yvess"
  ];

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      nur =
        import
          (
            builtins.fetchTarball
              "https://github.com/nix-community/NUR/archive/master.tar.gz"
          )
          {
            inherit pkgs;
          };
    };
  };

  programs.zsh.enable = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = [
    pkgs.vim
  ];

  services.yabai = {
    enable = true;
    config = {
      focus_follows_mouse = "autoraise";
      mouse_follows_focus = "on";
      window_placement = "second_child";
      window_opacity = "off";
      # top_padding = 10;
      # bottom_padding = 10;
      # left_padding = 10;
      # right_padding = 10;
      window_gap = 10;
      layout = "bsp";
    };

    extraConfig = ''
      yabai -m rule --add app='System Settings' manage=off
      yabai -m rule --add app="Discord" manage=off
      yabai -m rule --add app="WhatsApp" manage=off
      yabai -m rule --add app="Finder" manage=off
      yabai -m rule --add app="Thunderbird" manage=off
      yabai -m rule --add app="IINA" manage=off
    '';
  };

  services.skhd = {
    enable = true;
    skhdConfig = "${builtins.readFile ./skhdrc}";
  };

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;
}
