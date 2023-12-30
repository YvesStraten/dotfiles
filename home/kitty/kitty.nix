{ pkgs, ... }: {
  programs.kitty = {
    enable = true;
    font.name = "JetBrainsMono Nerd Font";
    font.size = 23;
    shellIntegration.enableZshIntegration = true;
    theme = "Gruvbox Dark";
  };
}
