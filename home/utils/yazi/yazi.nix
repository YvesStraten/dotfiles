{ pkgs, ... }: {
  programs.yazi = {
    enable = true;
    enableZshIntegration = true;
  };

  home.file.".config/yazi/yazi.toml" = {
    source = ./config/yazi.toml;
  };
}
