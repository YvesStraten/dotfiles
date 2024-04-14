{ pkgs, inputs, ... }:
let
  firefox = if pkgs.stdenv.isDarwin then pkgs.firefox-bin else pkgs.firefox;

  theme = pkgs.stdenvNoCC.mkDerivation {
    name = "Shina-fox";
    src = pkgs.fetchurl {
      url =
        "https://github.com/Shina-SG/Shina-Fox/releases/download/release/Shina.Fox.0.1.-.Frieren.Edition.7z";
      sha256 = "uLQjluSuz8iXO9M0AQE4N1C6qHv9wvO8Dv4TjweLuRw=";
    };

    sourceRoot = ".";

    buildInputs = [ pkgs.p7zip ];

    installPhase = ''
      mkdir -p $out
      7z x $src .
      cp -R * $out
    '';
  };
in {
  programs.firefox = {
    enable = true;
    package = firefox;
    policies = {
      DisableAppUpdate = true;
      DisablePocket = true;
      DisableTelemetry = true;
    };
    profiles.yvess = {
      isDefault = true;
      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        react-devtools
        darkreader
        ublock-origin
        sidebery
      ];
      settings = {
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "layers.acceleration.force-enabled" = true;
        "gfx.webrender.all" = true;
        "svg.context-properties.content.enabled" = true;

        "pdfjs.disabled" = true;

        "browser.aboutConfig.showWarning" = false;
      };
      userChrome = ''
        ${builtins.readFile "${theme}/userChrome.css"}
      '';
    };
  };
}
