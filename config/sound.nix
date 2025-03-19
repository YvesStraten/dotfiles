{
  config,
  options,
  lib,
  ...
}:
let
  cfg = config.custom.sound;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.sound.enable = mkEnableOption "Enable sound" // {
    default = true;
  };

  config = mkIf cfg.enable (mkMerge [
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
  ]);
}
