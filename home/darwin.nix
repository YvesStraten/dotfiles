{
  inputs,
  user,
  ...
}: let 
	hmModule = (name: inputs.${name}.homeManagerModules.default);
in {
  imports = let 
		nvchad = hmModule "nvchad";
	in [
		nvchad
    ../config/alt-tab/alt-tab.nix
    # Main stuff
    ./theming-darwin.nix
    ./bin/default.nix

    ./dev
    ./utils
    ./pass
  ];

	programs.nvchad.enable = true;

  nixpkgs.config.allowUnfree = true;

  home = {
    username = user;
    homeDirectory = "/Users/${user}";
    stateVersion = "22.11"; # Please read the comment before changing.

    sessionPath = [
      "$HOME/.config/emacs/bin"
      "/Applications/XAMPP/bin"
    ];
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.man = {
    enable = true;
    generateCaches = true;
  };
}
