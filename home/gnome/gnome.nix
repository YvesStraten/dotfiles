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
	];
}
