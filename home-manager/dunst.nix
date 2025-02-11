{
  config,
  options,
  lib,
  ...
}:
let
  cfg = config.custom.dunst;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.dunst.enable = mkEnableOption "Enable dunst";

  config = mkIf cfg.enable {
    services.dunst = {
      enable = true;
      settings = {
        global = {
          follow = "mouse";
          indicate_hidden = true;

          offset = "10x50";

          notification_height = 0;

          separator_height = 2;

          padding = 8;
          horizontal_padding = 8;
          text_icon_padding = 0;
          frame_width = 2;

          frame_color = "#89B4FA";
          separator_color = "frame";

          sort = true;
          idle_threshold = 120;
          font = "monospace 10";
          line_height = 0;
          markup = "full";
          alignment = "left";
          vertical_alignment = "center";
          show_age_threshold = 60;
          word_wrap = true;
          stack_duplicates = true;
          hide_duplicate_count = false;

          show_indicators = true;

          min_icon_size = 0;
          max_icon_size = 64;
          dmenu = "/usr/bin/wofi - p dunst:";
          browser = "brave --new-tab";

          title = "Dunst";
          class = "Dunst";

          corner_radius = 10;
          timeout = 5;
        };

        urgency_low = {
          background = "#1E1E2E";
          foreground = "#CDD6F4";
        };

        urgency_normal = {
          background = "#1E1E2E";
          foreground = "#CDD6F4";
        };

        urgency_critical = {
          background = "#1E1E2E";
          foreground = "#CDD6F4";
          frame_color = "#FAB387";
        };
      };
    };
  };
}
