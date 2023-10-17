{ pkgs
, lib
, config
, ...
}:
let
  theme = pkgs.stdenv.mkDerivation {
    name = "sddm-sugar-dark";
    src = pkgs.fetchFromGitHub
      {
        owner = "MarianArlt";
        repo = "sddm-sugar-dark";
        rev = "ceb2c455663429be03ba62d9f898c571650ef7fe";
        sha256 = "0153z1kylbhc9d12nxy9vpn0spxgrhgy36wy37pk6ysq7akaqlvy";
      };

    installPhase = ''
      mkdir -p $out
      cp -R ./* $out/
    '';
  };
in
{
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
    enableNvidiaPatches = true;
  };

  services.xserver.displayManager.sddm = {
    enable = true;
    theme = "${theme}";
  };

  environment.systemPackages = with pkgs; [
    libsForQt5.qt5.qtquickcontrols2
    libsForQt5.qt5.qtgraphicaleffects
    config.nur.repos.mikilio.xwaylandvideobridge-hypr
  ];

  sound.enable = false;
  security.rtkit.enable = true;
  security.polkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
}
