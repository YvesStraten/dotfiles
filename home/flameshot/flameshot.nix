{ pkgs, lib, ... }: {
  services.flameshot = {
    enable = true;
    # settings = {
    #   General = {
    #     savePath = "${xdg-user-dir PICTURES}/ScreenShots";
    #   };
    # };
  };
}
