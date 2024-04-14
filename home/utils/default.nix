{ pkgs, config, ... }:

{
  imports = [
    ../../config/rclone/rclone.nix
    ./general.nix
    ./firefox.nix
    ./yazi/yazi.nix
    ./mpv.nix
    ./zathura.nix
    ./thunderbird.nix
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
