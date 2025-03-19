{
  config,
  options,
  pkgs,
  lib,
  ...
}:
let
  inherit (lib) mkMerge;
in
{
  config = mkMerge [
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
  ];
}
