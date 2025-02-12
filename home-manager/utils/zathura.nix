{
  config,
  options,
  inputs,
  lib,
  ...
}:
let
  cfg = config.custom.zathura;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.zathura.enable = mkEnableOption "Enable zathura";

  config = mkIf cfg.enable {
    programs.zathura = {
      enable = true;
      options = {
        adjust-open = "best-fit";
        smooth-scroll = "true";
        selection-clipboard = "clipboard";
      };

      extraConfig = "${builtins.readFile "${inputs.zathura-dracula}/zathurarc"}";
    };
  };

}
