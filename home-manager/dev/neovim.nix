{ options
, config
, lib
, self
, pkgs
, ...
}:
let
  cfg = config.custom.nvim;
  inherit (lib) mkIf mkEnableOption;
in
{
  options.custom.nvim.enable = mkEnableOption "Enable neovim";

  config = mkIf cfg.enable {
    home = {
      packages = [ self.packages.${pkgs.system}.nvim ];
      sessionVariables = {
        EDITOR = "nvim";
      };
    };
  };
}
