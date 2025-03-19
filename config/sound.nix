{
  config,
  options,
  lib,
  ...
}:
let
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  config = mkMerge [
    {
      security.rtkit.enable = true;

      services = {
        pipewire = {
          enable = true;
          alsa.enable = true;
          alsa.support32Bit = true;
          pulse.enable = true;
          wireplumber.enable = true;
        };

        pulseaudio.enable = false;
      };
    }
  ];
}
