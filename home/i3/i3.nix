{ pkgs, lib, ... }:

{
  imports = [ ../dunst/dunst.nix ../rofi/rofi.nix ../polybar/polybar.nix ../picom/picom.nix ];

  environment.pathsToLink = [ "/libexec" ];

  services.xserver = {
    enable = true;

    desktopManager = { xterm.enable = false; };

    displayManager = { defaultSession = "none+i3"; };
    windowManager.i3 = {
      package = pkgs.i3-gaps;
      enable = true;
      extraPackages = with pkgs; [
        autotiling
        pamixer
        pulseaudio
        feh
        pywal
        picom
        maim
        autorandr
        xdotool
        xclip
      ];
    };
  };

  sound.enable = false;
  security.rtkit.enable = true;
  security.polkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;

    wireplumber.configPackages = [
      (pkgs.writeTextDir "share/wireplumber/bluetooth.lua.d/51-bluez-config.lua"
        "	bluez_monitor.properties = {\n		[\"bluez5.enable-sbc-xq\"] = true,\n		[\"bluez5.enable-msbc\"] = true,\n		[\"bluez5.enable-hw-volume\"] = true,\n		[\"bluez5.headset-roles\"] = \"[ hsp_hs hsp_ag hfp_hf hfp_ag ]\"\n	}\n")
    ];
  };

  hm = {
    services = {
      kdeconnect = {
        enable = true;
        indicator = true;
      };

      udiskie = {
        enable = true;
        automount = true;
        tray = "never";
      };
    };

    # Required to get targets such as tray to work
    xsession.enable = true;

    home.file.".config/i3/config" = {
      source = ./config;
    };
  };
}
