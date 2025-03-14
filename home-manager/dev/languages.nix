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

    home.packages = with pkgs;
      let
        # CUDA
      whisper-warnings = openai-whisper.override { torch = python3.pkgs.torch-bin; };

      # Some closures seem to conflict, I am not insanely good at python to fix this
      whisper-no-warnings = whisper-warnings.overridePythonAttrs(o: {
        catchConflicts = false;
      });
      in
      [
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
      typescript
      whisper-no-warnings

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
