{
  config,
  options,
  pkgs,
  lib,
  ...
}: let
  cfg = config.custom.office;
  inherit (lib) mkEnableOption mkIf mkMerge;
in {
  options.custom.office = {
    enable = mkEnableOption "Enable office tools";
    libreoffice.enable = mkEnableOption "Enable libreoffice" // {default = cfg.enable;};
    latex.enable = mkEnableOption "Enable latex" // {default = cfg.enable;};
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf cfg.libreoffice.enable {
      home.packages = with pkgs; [
        libreoffice
        zotero
      ];
    })

    (mkIf cfg.latex.enable {
      home.packages = with pkgs; [
        texlive.combined.scheme-medium
      ];
    })
  ]);
}
