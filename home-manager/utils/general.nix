{
  config,
  options,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.custom.general;
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types
    ;

  packages = with pkgs; [
    tesseract
    rsync
    rclone
    pandoc
    nix-prefetch-scripts
    inkscape
    btop
    spotify
    qalculate-qt
    kdePackages.elisa
  ];
in
{
  options.custom.general = {
    enable = mkEnableOption "Enable general utils";
    extraPackages = mkOption {
      type = types.listOf types.package;
      default = [ ];
      description = ''
        Packages to include in general utils
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages = packages ++ cfg.extraPackages;
  };
}
