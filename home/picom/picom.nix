{ pkgs, ... }:

{
  hm = {
    services.picom = {
      enable = true;
      package = pkgs.yvess.picom-pijulius;
    };

    xdg.configFile."picom/picom.conf" = {
      source = ./picom.conf;
    };
  };
}
