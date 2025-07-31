{ config
, options
, pkgs
, lib
, ...
}:
let
  cfg = config.custom.office;
  inherit (lib) mkEnableOption mkIf mkMerge;
in
{
  options.custom.office = {
    enable = mkEnableOption "Enable office tools";
    libreoffice.enable = mkEnableOption "Enable libreoffice" // { default = cfg.enable; };
    latex.enable = mkEnableOption "Enable latex" // { default = cfg.enable; };
    typst.enable = mkEnableOption "Enable typst" // { default = cfg.enable; };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        pdfarranger
        (aspellWithDicts (ds:
          with ds; [
            en
            id
            de
            en-computers
            en-science
          ]))
      ];
    }

    (mkIf cfg.libreoffice.enable {
      home.packages = with pkgs;
        [
          libreoffice
          zotero
          hunspell
        ]
        ++ (with pkgs.hunspellDicts; [
          en_US-large
          de_DE
          en_GB-large
          it_IT
          id_ID
        ]);
    })

    (mkIf cfg.latex.enable {
      home.packages = with pkgs; [
        texlive.combined.scheme-medium
      ];
    })

    (mkIf cfg.typst.enable {
      home.packages = [
        pkgs.typst
      ];
    })
  ]);
}
