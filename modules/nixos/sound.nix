{
  config,
  pkgs,
  lib,
  ...
}
: {
  sound.mediaKeys = {
    enable = true;
  };

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
}
