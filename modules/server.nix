{
	shell,
	pkgs, 
	user, 
  ...
}: {
  programs.${shell}.enable = true;

  fonts.packages = with pkgs; [ corefonts ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users = {
    defaultUserShell = pkgs.${shell};
    users.${user} = {
      isNormalUser = true;
      description = "${user}";
      extraGroups = [
        "networkmanager"
        "wheel"
        "audio"
        "libvirtd"
        "docker"
        "dialout"
        "fuse"
      ];
    };
  };

  services.openssh.enable = true;

	environment.systemPackages = with pkgs; [
	vim 
	wget

	];

  imports = [
    ./nixos/bootloader.nix
    ./nixos/networking.nix
    ./nixos/settings.nix
    ./nixos/time.nix
    ../overlays/default.nix
  ];
}
