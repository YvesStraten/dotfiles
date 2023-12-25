{pkgs, ...}: {
  programs.firefox = {
    enable = true;
    package = pkgs.firefox-bin;
    profiles.yvess = {
      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        darkreader
        ublock-origin
      ];
      settings = {
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "layers.acceleration.force-enabled" = true;
        "gfx.webrender.all" = true;
        "svg.context-properties.content.enabled" = true;
      };
      # Copyright goes to Miguel Avila for
      # Userchrome and content
      userChrome = ''
			${builtins.readFile ./userChrome.css}
      '';

      userContent = ''
			${builtins.readFile ./userContent.css}
      '';
    };
  };
}
