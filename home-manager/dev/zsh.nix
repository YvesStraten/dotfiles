{
  config,
  options,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.custom.zsh;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.zsh.enable = mkEnableOption "Enable zsh";

  config = mkMerge [
    (mkIf cfg.enable {
      programs.zsh = {
        enable = true;
        autosuggestion.enable = true;
        syntaxHighlighting.enable = true;
        enableCompletion = true;

        shellAliases = {
          cat = "${lib.getExe pkgs.bat} \"$@\"";
        };

        initExtraFirst = ''
          ZSH_DISABLE_COMPFIX=true
        '';

        oh-my-zsh = {
          enable = true;
          plugins = [
            "git"
            "common-aliases"
          ];
        };
      };
      custom.starship.enable = true;
    })
  ];
}
