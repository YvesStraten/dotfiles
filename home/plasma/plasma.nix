{ pkgs, config, ... }: {
  services.xserver = {
    enable = true;
    displayManager.sddm = {
      enable = true;
      theme = "${pkgs.yvess.sugar-dark}";
    };
    desktopManager.plasma5.enable = true;
  };

  environment.systemPackages = with pkgs; [
    config.nur.repos.mikilio.xwaylandvideobridge-hypr
  ];

  environment.plasma5.excludePackages = with pkgs.libsForQt5; [
    elisa
    gwenview
    dolphin
    konsole
    okular
    oxygen
    khelpcenter
  ];
}
