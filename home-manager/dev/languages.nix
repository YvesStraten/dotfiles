{ config
, options
, pkgs
, lib
, ...
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

    home.packages = with pkgs; let
      # CUDA
      packageOverrides = self: super: {
        torch = super.torch-bin;
      };
    in
    [
      ((python311.override { inherit packageOverrides; }).withPackages (
        ps:
          with ps; [
            yt-dlp
            spotdl
            pygments
            tkinter
            # jupyter
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
      openai-whisper

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
