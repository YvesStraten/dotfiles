{
  config,
  options,
  lib,
  ...
}:
let
  cfg = config.custom.nvidia;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.nvidia.enable = mkEnableOption "NVIDIA gpu";

  config = mkIf cfg.enable {

    services.xserver = {
      enable = true;
      xkb.layout = "us";
      videoDrivers = [ "nvidia" ];
    };

    hardware = {
      graphics.enable = true;

      nvidia = {
        package = config.boot.kernelPackages.nvidiaPackages.latest;
        modesetting.enable = true;
        open = true;
      };
    };
  };
}
