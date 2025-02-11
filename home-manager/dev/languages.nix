{
  config,
  options,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.custom.languages;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.languages.enable = mkEnableOption "Enable languages";

  config = mkIf cfg.enable {
    home.sessionVariables = {
      DOTNET_ROOT = "${pkgs.dotnet-sdk}";
    };

    home.packages = with pkgs; [
      (python311.withPackages (
        ps: with ps; [
          yt-dlp
          spotdl
          pygments
          tkinter
          # jupyter
          pillow
        ]
      ))

      texlive.combined.scheme-medium
      maven
      nodejs
      rustup
      yarn
      openjdk
      dotnet-sdk

      unzip
      sshfs
      ffmpeg
      neofetch
      (hiPrio gcc)
      clang
      gnumake
      drawio
    ];

    programs.direnv = {
      enable = true;
    };
  };
}
