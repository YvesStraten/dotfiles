{ pkgs, ... }: {
  services.picom = {
    enable = true;
    backend = "egl";
    extraArgs = [ "--experimental-backends" ];
    fade = true;
  };

  # home.file.".config/picom/picom.conf" = {
  #   source = ./picom.conf;
  # };
}
