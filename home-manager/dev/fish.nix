{
  config,
  options,
  pkgs,
  lib,
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
            src = tide.src;
          }

          {
            name = "sponge";
            src = sponge.src;
          }
        ];
      };

      custom = {
        # starship.enable = true;
        zoxide.enable = true;
      };
    })
  ];
}
