{ config
, options
, pkgs
, lib
, ...
}:
let
  cfg = config.custom.pass;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.pass = {
    enable = mkEnableOption "Enable password store" // {
      default = true;
    };
    wayland = mkEnableOption "Using wayland";
    x = mkEnableOption "Using x";
  };

  config = mkIf cfg.enable {
    programs = {
      password-store = {
        enable = true;
        package = pkgs.pass.withExtensions (
          exts: with exts; [
            pass-otp
            pass-import
            pass-update
          ]
        );
      };
      gpg.enable = true;
    };

    services = {
      gpg-agent = {
        enable = true;
        pinentry.package = pkgs.pinentry-gnome3;
      };
    };

    home.packages = with pkgs; [
      qtpass
      (mkIf cfg.wayland wl-clipboard)
      (mkIf cfg.x xclip)
    ];
  };
}
