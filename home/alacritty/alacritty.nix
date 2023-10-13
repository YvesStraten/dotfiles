{ pkgs, config, ... }: {
  programs.alacritty = {
    enable = true;
    settings = {
      font = {
        normal.family = "JetBrainsMono NF";
        bold.family = "JetBrainsMono NF";
        italic.family = "JetBrainsMono NF";
        bold_italic.family = "JetBrainsMono NF";

        size = 15.0;
      };

      window = {
        opacity = 0.8;
      };

      colors = with config.colorScheme.colors; {
        primary = {
          background = "0x${base00}";
          foreground = "0x${base06}";
        };
      };
    };
  };
}
