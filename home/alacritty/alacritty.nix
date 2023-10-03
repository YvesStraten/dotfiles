{ pkgs, ... }: {
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
    };
  };
}
