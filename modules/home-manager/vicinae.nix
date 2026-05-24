{ inputs, ... }:
{
  flake.homeModules.vicinae = _: {
    imports = [
      inputs.vicinae.homeManagerModules.default
    ];

    services.vicinae = {
      enable = true;
      systemd = {
        enable = true; # default: false
        autoStart = true; # default: false
        environment = {
          USE_LAYER_SHELL = 1;
        };
      };
    };
  };
}
