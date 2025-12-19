{
  config,
  options,
  inputs,
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

    enablePwas = mkEnableOption "Enable pwas";
  };

  config = mkIf cfg.enable {
    home.packages = mkIf cfg.enablePwas [ pkgs.firefoxpwa ];
    programs.firefox = {
      enable = true;
      package = cfg.package;
      nativeMessagingHosts = mkIf cfg.enablePwas [
        pkgs.firefoxpwa
      ];

      policies = {
        DisableAppUpdate = true;
        DisablePocket = true;
        DisableTelemetry = true;
      };
      profiles.yvess = {
        isDefault = true;
        extensions.packages =
          with inputs.firefox-addons.packages.${pkgs.stdenv.hostPlatform.system};
          [
            react-devtools
            browserpass
            redirector
            darkreader
            ublock-origin
            zotero-connector
            yomitan
            adaptive-tab-bar-colour
          ]
          ++ (if cfg.enablePwas then [ pwas-for-firefox ] else [ ]);
        settings = {
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "layers.acceleration.force-enabled" = true;
          "gfx.webrender.all" = true;
          "svg.context-properties.content.enabled" = true;

          "pdfjs.disabled" = true;
          "sidebar.verticalTabs" = true;

          "browser.aboutConfig.showWarning" = false;
        };
        userChrome = '''';
      };
    };
  };
}
