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

    environment.variables = {
      "__GL_SHADER_DISK_CACHE_SKIP_CLEANUP" = 1;
      "__GL_SHADER_DISK_CACHE_SIZE" = 100000000000;
      "GL_SHADER_DISK_CACHE" = 1;
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
