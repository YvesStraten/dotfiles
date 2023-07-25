{
  config,
  pkgs,
  lib,
  ...
}
: let
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-GO
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec "$@"
  '';
in {
  environment.systemPackages = with pkgs; [
    nvidia-offload
  ];

  # Configure keymap in X11
  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "";
  };

  services.xserver.videoDrivers = ["nvidia"];
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
      intel-media-driver
    ];
    extraPackages32 = with pkgs; [
      libva
      vaapiIntel
    ];
  };
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;
  hardware.nvidia.modesetting.enable = true;

  hardware.nvidia.prime = {
    offload.enable = true;

    intelBusId = "PCI:0:2:0";
    nvidiaBusId = "PCI:1:0:0";
  };

  specialisation = {
    external-display.configuration = {
      system.nixos.tags = ["external-display"];
      hardware.nvidia.prime.offload.enable = lib.mkForce false;
      hardware.nvidia.powerManagement.enable = lib.mkForce false;
    };
  };
}
