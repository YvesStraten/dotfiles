{ pkgs, lib, ... }: {
  custom.fish.enable = true;
  custom.kitty.enable = true;
  custom.firefox.enable = true;
  custom.utils.enable = lib.mkForce false;
  custom.general.enable = true;
  custom.thunderbird.enable = lib.mkForce false;
  custom.theming = {
      enable = true;
      gtk.enable = false;
      qt.enable = false;
    };
}
