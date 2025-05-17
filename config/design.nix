{ config
, options
, pkgs
, lib
, ...
}:
let
  cfg = config.custom.design;
  inherit (lib) mkEnableOption mkIf;
in
{
  options.custom.design.enable = mkEnableOption "Enable design tools";

  config = mkIf cfg.enable {
    programs.obs-studio = {
      enable = true;
      enableVirtualCamera = true;
      plugins = with pkgs.obs-studio-plugins; [
        wlrobs
        obs-backgroundremoval
        obs-pipewire-audio-capture
        droidcam-obs
      ];
    };

    environment.systemPackages = with pkgs; [
      gimp3-with-plugins
      audacity
      darktable
    ];
  };
}
