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
      maven
      openai-whisper
      yt-dlp
      spotdl
      nodejs
      rustup
      yarn
      openjdk
      dotnet-sdk
      typescript

      unzip
      sshfs
      ffmpeg
      neofetch
      clang
      gnumake
      drawio
    ];

    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
  };
}
