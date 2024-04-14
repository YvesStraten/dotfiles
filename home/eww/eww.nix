{pkgs, ...}: {
  hm.programs.eww = {
    enable = true;
    package = pkgs.eww;
    configDir = ./config;
  };
}
