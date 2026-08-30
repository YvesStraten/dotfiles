{ config, ... }:
{
  flake.homeModules.core =
    {
      osConfig,
      lib,
      ...
    }:
    let
      inherit (osConfig.custom.constants) user;
      inherit (lib) mkMerge;
    in
    {
      config = mkMerge [
        {
          home = {
            username = user;
            homeDirectory = "/home/${user}";
            stateVersion = "22.11"; # Please read the comment before changing.
            preferXdgDirectories = true;

            sessionPath = [ "$HOME/.local/bin" ];
            shell = {
              enableBashIntegration = true;
              enableZshIntegration = true;
              enableFishIntegration = true;
            };
          };
        }

        {
          # Let Home Manager install and manage itself.
          programs.home-manager.enable = true;
          xdg.userDirs = {
            enable = true;
            createDirectories = true;
            setSessionVariables = false;
          };
        }
      ];
    };
}
