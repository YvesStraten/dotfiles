{
  flake.nixosModules.gui =
    {
      config,
      pkgs,
      ...
    }:
    {
      programs.obs-studio = {
        enable = true;
        enableVirtualCamera = true;
        package = pkgs.obs-studio.override { cudaSupport = true; };
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
        digikam
        krita
      ];
    };
}
