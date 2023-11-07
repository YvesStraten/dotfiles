{ pkgs, config, ... }: {
  services.xserver = {
    enable = true;
    displayManager.sddm = {
      enable = true;
      theme = "${pkgs.yvess.sugar-dark}";
    };
    desktopManager.plasma5.enable = true;
  };

  sound.enable = false;
  security.rtkit.enable = true;
  security.polkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  environment.systemPackages = with pkgs; [
    config.nur.repos.mikilio.xwaylandvideobridge-hypr
  ]
  ++ (with pkgs.libsForQt5; [
    bismuth
  ]);

  environment.plasma5.excludePackages = with pkgs.libsForQt5;
    [
      elisa
      gwenview
      dolphin
      konsole
      okular
      oxygen
      khelpcenter
    ];
}
