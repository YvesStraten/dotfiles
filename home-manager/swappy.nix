{
  lib,
  options,
  config,
  pkgs,
  ...
}:
let
  cfg = config.custom.swappy;
  inherit (lib) mkEnableOption mkIf;
in
{
  options.custom.swappy.enable = mkEnableOption "Enable swappy";

  config = mkIf cfg.enable {
    home = {
      packages = [ pkgs.swappy ];

      file.".config/swappy/config" = {
        text = ''
          [Default]
          save_dir=$HOME/Pictures
          save_filename_format=swappy-%Y%m%d-%H%M%S.png
          show_panel=true
          line_size=5
          text_size=20
          text_font=sans-serif
          paint_mode=brush
          early_exit=false
          fill_shape=false
        '';
      };

    };
  };
}
