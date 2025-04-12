{
  config,
  options,
  lib,
  ...
}:
let
  cfg = config.custom.utils;
  inherit (lib) mkEnableOption mkIf mkMerge;
in
{
  imports = [
    ./yazi/yazi.nix
    ./firefox.nix
    ./zathura.nix
    ./mpv.nix
    ./thunderbird.nix
    ./kde-connect.nix
    ./rclone.nix
    ./general.nix
    ./alt-tab.nix
    ./syncthing.nix
    ./office.nix
  ];

  options.custom.utils.enable = mkEnableOption "Enable utils" // {
    default = true;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      custom = {
        yazi.enable = true;
        firefox = {
          enable = true;
          enablePwas = true;
        };
        zathura.enable = true;
        mpv.enable = true;
        thunderbird.enable = true;
        syncthing.enable = true;
        general.enable = true;
        kde-connect.enable = true;
      };

      programs.nh.enable = true;

      services.rclone-bisync = {
        enable = true;
        enableTimers = false;
        bisyncs = {
          onedrive = {
            remotePath = "Onedrive:Uni";
            localPath = "${config.home.homeDirectory}/Gdrive/Uni";
          };
        };
      };
    })
  ];
}
