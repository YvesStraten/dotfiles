{
  config,
  options,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.custom.firefox;
  inherit (lib)
    mkMerge
    mkEnableOption
    mkIf
    mkOption
    types
    ;
in
{
  options.custom.firefox = {
    enable = mkEnableOption "Enable firefox";
    package = mkOption {
      type = types.package;
      default = (if pkgs.stdenv.isDarwin then null else pkgs.firefox);
      description = ''
        Packaged firefox to use
      '';
    };
  };

  config = mkIf cfg.enable {
    programs.firefox = {
      enable = true;
      package = cfg.package;
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
        userChrome = '''';
      };
    };
  };
}
