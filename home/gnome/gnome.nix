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
	
	environment.gnome.excludePackages = (with pkgs; [
	gnome-photos 
	gnome-tour
	]) ++ (with pkgs.gnome; [
	gnome-terminal 
	epiphany
	geary 
	tali
	iagno
	hitori 
	atomix
	]);
}
