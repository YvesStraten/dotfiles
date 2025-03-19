{ lib, ... }: {
  custom.fish.enable = true;
  custom.utils.enable = lib.mkForce false;
  custom.thunderbird.enable = lib.mkForce false;
}
