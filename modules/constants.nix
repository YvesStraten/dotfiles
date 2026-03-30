{ lib, ... }:
{
  flake.nixosModules.core = _: {
    options.custom.constants = lib.mkOption {
      type = lib.types.attrsOf lib.types.unspecified;
      default = { };
    };
  };
}
