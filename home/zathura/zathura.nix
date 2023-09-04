{pkgs, ...}: {
  programs.zathura = {
    enable = true;
    options = {
      selection-clipboard = "clipboard";
    };
    extraConfig = "include theme";
  };

  home.file.".config/zathura/theme" = {
    source = ./themes/catppuccin-mocha;
  };
}
