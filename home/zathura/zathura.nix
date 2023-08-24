{ pkgs, ... }: {
  programs.zathura = {
    enable = true;
    options = {
      selection-clipboard = "clipboard";
    };
    extraConfig = "include theme";
  };
}
