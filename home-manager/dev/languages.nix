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

    home.packages =
      with pkgs;
      let
        # CUDA
        packageOverrides = self: super: {
          # This is needed as triton in torch has a different version than whisper
          torch = super.torch-bin.override (o: {
            triton = super.triton;
          });
          openai-whisper = super.openai-whisper.override (o: {
            triton = super.triton;
          });
        };
      in
      [
        ((python312.override { inherit packageOverrides; }).withPackages (
          ps: with ps; [
            openai-whisper
            yt-dlp
            spotdl
            pygments
            pillow
          ]
        ))

        maven
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
