{
  pkgs,
  inputs,
  ...
}: let
  firefox =
    if pkgs.stdenv.isDarwin
    then null 
    else pkgs.firefox;
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
        # sidebery
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
      '';
    };
  };
}
