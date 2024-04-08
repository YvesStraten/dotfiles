{ pkgs, ... }: {
  programs.yazi = {
    enable = true;
    enableZshIntegration = true;
  };

  home.file.".config/yazi/yazi.toml" = { source = ./config/yazi.toml; };

  home.file.".config/yazi/keymap.toml" = { source = ./config/keymap.toml; };
}
