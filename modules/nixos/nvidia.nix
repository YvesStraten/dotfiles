{
  config,
  pkgs,
  lib,
  ...
}: {
  services.xserver = {
    enable = true;
    xkb.layout = "us";
  };

  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.latest;
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
  };

  hardware.nvidia.modesetting.enable = true;
  hardware.nvidia.open = true;
}
