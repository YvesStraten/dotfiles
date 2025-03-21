{
  options,
  config,
  lib,
  self,
}: let
  cfg = config.custom.nvim;
  inherit (lib) mkIf mkEnableOption;
in {
  options.custom.nvim.enable = mkEnableOption "Enable neovim";

  config = mkIf cfg.enable {
    home.packages = [self.packages.nvim];
  };
}
