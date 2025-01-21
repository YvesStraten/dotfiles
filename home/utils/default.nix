{
  pkgs,
  config,
  ...
}: {
  imports = [
		../../config/rclone/rclone.nix
		../../config/alt-tab/alt-tab.nix
    ./general.nix
    ./firefox.nix
    ./yazi/yazi.nix
    # ./mpv.nix
    ./zathura.nix
    ./thunderbird.nix
    ./alt-tab.nix
  ];

	programs.nh.enable = true;

  services.rclone-bisync = {
    enable = true;
    bisyncs = {
      gdrive = {
        remotePath = "Gdrive:School/Uni";
        localPath = "${config.home.homeDirectory}/Gdrive/Uni";
      };
    };
  };
}
