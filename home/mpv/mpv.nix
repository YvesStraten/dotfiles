{ pkgs, ... }: {
  programs.mpv = {
    enable = true;
    config = {
      sub-auto = "fuzzy";
      sub-font = "RobotoMono";
      sub-bold = true;
      tscale = "oversample";
      interpolation = true;
      save-position-on-quit = true;
    };
    defaultProfiles = [
      "gpu-hq"
    ];
  };
}
