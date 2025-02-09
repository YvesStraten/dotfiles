{ config, options, lib, ... }:
let
  cfg = config.hm.custom.udisks;
  inherit (lib) mkMerge mkEnableOption mkIf;
in mkIf cfg.enable {
  services.udisks2.enable = true;
}
