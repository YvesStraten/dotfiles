{
  pkgs,
  config,
  ...
}: {
  services.xserver = {
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
  };

  environment.systemPackages = with pkgs; [
    gnomeExtensions.appindicator
    gnomeExtensions.pano
    gnomeExtensions.forge
    gnomeExtensions.espresso
    gnomeExtensions.dock-from-dash
    gnomeExtensions.docker
    gnomeExtensions.gsconnect
    gnome.gnome-tweaks
  ];

  environment.gnome.excludePackages =
    (with pkgs; [
      gnome-photos
      gnome-tour
    ])
    ++ (with pkgs.gnome; [
      gnome-terminal
      gnome-music
      epiphany
      geary
      tali
      iagno
      hitori
      atomix
    ]);
}
