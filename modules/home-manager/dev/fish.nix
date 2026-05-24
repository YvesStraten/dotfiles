{ lib, ... }:
{
  flake.homeModules.dev =
    {
      config,
      pkgs,
      ...
    }:
    let
      cfg = config.custom.fish;
      inherit (lib) mkMerge mkEnableOption mkIf;
    in
    {
      options.custom.fish.enable = mkEnableOption "Enable fish";

      config = mkMerge [
        (mkIf cfg.enable {
          programs.fish = {
            enable = true;
            shellAliases = {
              cd = "z";
              ga = "git add";
              gc = "git commit";
              ".." = "cd ..";
              "..." = "cd ../..";
            };

            shellInit = ''
              set -g fish_greeting
            '';

            plugins = with pkgs.fishPlugins; [
              {
                name = "tide";
                inherit (tide) src;
              }

              {
                name = "sponge";
                inherit (sponge) src;
              }
            ];
          };
        })
      ];
    };
}
