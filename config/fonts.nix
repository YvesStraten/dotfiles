{
  config,
  options,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.custom.fonts;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.fonts.enable = mkEnableOption "Enable fonts";

  config = mkIf cfg.enable (mkMerge [
    {
      fonts = {
        fonts = with pkgs; [
          corefonts # Microsoft free fonts
          dejavu_fonts
          source-code-pro
          source-sans-pro
          source-serif-pro
          unifont # some international languages
        ];
        fontconfig = {
          antialias = true;
          cache32Bit = true;
          hinting.enable = true;
          hinting.autohint = true;
          defaultFonts = {
            monospace = [ "Source Code Pro" ];
            sansSerif = [ "Source Sans Pro" ];
            serif = [ "Source Serif Pro" ];
          };
        };
      };
    }
  ]);
}
