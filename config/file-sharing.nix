{
  config,
  options,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.custom.file-sharing;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.file-sharing.enable = mkEnableOption "Enable file sharing" // {
    default = true;
  };

  config = mkIf cfg.enable {
    services = {
      gvfs.enable = true;
      samba = {
        enable = true;
        openFirewall = true;
      };
      avahi = {
        enable = true;
        nssmdns4 = true;
        publish.enable = true;
      };
    };

    # To make SMB mounting easier on the command line
    environment.systemPackages = with pkgs; [
      cifs-utils
    ];
  };
}
