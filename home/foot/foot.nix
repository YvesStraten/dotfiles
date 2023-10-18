{ pkgs, ... }: {
  programs.foot = {
    enable = true;
    settings = {
      main = {
        font = "JetBrainsMono NF:size=15";
      };

      mouse = {
        hide-when-typing = "yes";
      };
    };
  };
}
