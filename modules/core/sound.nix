{
  config,
  pkgs,
  lib,
  ...
}
: {
  sound.enable = false;
  security.rtkit.enable = true;
  security.polkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  sound.mediaKeys = {
    enable = true;
  };

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
}
