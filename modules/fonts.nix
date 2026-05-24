_: {
  flake.nixosModules.fonts =
    { pkgs, ... }:
    {
      fonts = {
        enableDefaultPackages = true;
        enableGhostscriptFonts = true;
        packages = with pkgs; [
          corefonts # Microsoft free fonts
          inter
          dejavu_fonts
          source-code-pro
          nerd-fonts.fira-code
          source-sans-pro
          noto-fonts
          noto-fonts-color-emoji
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
    };
}
