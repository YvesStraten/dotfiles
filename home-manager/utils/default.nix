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
    ./rclone.nix
  ];

  options.custom.utils.enable = mkEnableOption "Enable utils" // {
    default = true;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      custom = {
        yazi.enable = true;
        firefox.enable = true;
        zathura.enable = true;
        mpv.enable = true;
        thunderbird.enable = true;
      };

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
    })
  ];
}
