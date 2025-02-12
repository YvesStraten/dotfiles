{
  pkgs,
  config,
  ...
}: {
  imports = [
		../../config/rclone/rclone.nix
    ./general.nix
  ];

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
