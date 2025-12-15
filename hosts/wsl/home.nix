{ pkgs, ... }:
{
  custom = {
    fish.enable = true;
    theming.enable = true;
    languages.enable = true;
    tmux.enable = true;
    utils.enable = false;
  };
}
