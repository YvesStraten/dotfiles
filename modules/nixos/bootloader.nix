{ config
, pkgs
, lib
, ...
}:
let
  sekiro = pkgs.stdenv.mkDerivation
    {
      name = "sekiro-grub";
      src = pkgs.fetchFromGitHub
        {
          owner = "semimqmo";
          repo = "sekiro_grub_theme";
          rev = "main";
          sha256 = "02gdihkd2w33qy86vs8g0pfljp919ah9c13cj4bh9fvvzm5zjfn1";
        };

      installPhase = ''
        mkdir -p $out
        cp -R ./Sekiro/* $out
      '';
    };
in
{
  boot.loader = {
    grub = {
      enable = true;
      efiSupport = true;
      device = "nodev";
      theme = sekiro;
    };
    efi.canTouchEfiVariables = true;
    efi.efiSysMountPoint = "/boot/";
  };
  boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback xpadneo ];
  boot.extraModprobeConfig = ''
    options v4l2loopback nr_devices=2 exclusive_caps=1,1 video_nr=0,1 card_label=v4l2lo0,v4l2lo1
  '';
}
