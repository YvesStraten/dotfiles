{ lib, ... }:
{
  flake.homeModules.dev =
    {
      config,
      pkgs,
      ...
    }:
    let
      cfg = config.custom.languages;
      inherit (lib) mkEnableOption mkIf;
    in
    {
      options.custom.languages.enable = mkEnableOption "Enable languages";

      config = mkIf cfg.enable {
        home.packages = with pkgs; [
          maven
          (openai-whisper.override (
            _:
            let
              triton = python313Packages.triton-cuda;
            in
            {
              inherit triton;
              torch = python313Packages.torch.override {
                inherit triton;
                cudaSupport = true;
              };
            }
          ))
          yt-dlp
          spotdl
          nodejs
          rustup
          yarn
          gradle-packages.gradle_9
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

        programs.java = {
          enable = true;
          package = pkgs.openjdk25;
        };
      };
    };
}
