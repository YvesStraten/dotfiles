{ pkgs
, inputs
, ...
}:
let
  firefox =
    if pkgs.stdenv.isDarwin
    then pkgs.firefox-bin
    else null;
in
{
  programs.firefox = {
    enable = true;
    package = firefox;
    policies = {
      AppAutoUpdate = false;
    };
    profiles.yvess = {
      isDefault = true;
      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        darkreader
        ublock-origin
        tree-style-tab
        sidebery
      ];
      settings = {
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "layers.acceleration.force-enabled" = true;
        "gfx.webrender.all" = true;
        "svg.context-properties.content.enabled" = true;

        "app.update.auto" = false;
        "app.update.silent" = true;
        "browser.aboutConfig.showWarning" = false;
      };
      userChrome = ''
        ${builtins.readFile "${pkgs.yvess.theme}/userChrome.css"}
      '';
    };
  };
}
