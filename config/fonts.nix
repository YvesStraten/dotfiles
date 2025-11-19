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
        enableDefaultPackages = true;
        enableGhostscriptFonts = true;
        packages = with pkgs; [
          corefonts # Microsoft free fonts
          dejavu_fonts
          source-code-pro
          source-sans-pro
          noto-fonts
          noto-fonts-emoji
          source-serif-pro
          unifont # some international languages
        ];

        fontconfig = {
          antialias = true;
          hinting = {
            enable = true;
            autohint = true;
            style = "full";
          };

          subpixel = {
            rgba = "rgb";
            lcdfilter = "default";
          };
        };
      };
    }
  ]);
}
