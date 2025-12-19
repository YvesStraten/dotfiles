{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    getExe
    mkOption
    mkIf
    mkMerge
    escapeShellArgs
    types
    ;
  cfg = config.services.jupyter-notebook;
  jupyterArgs = defaultJupyterArgs ++ cfg.extraOptions;
  defaultJupyterArgs = [
    "${getExe pkgs.jupyter}"
    "--no-browser"
  ];
in
{
  options = {
    services.jupyter-notebook = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enables jupyter notebook as a service
        '';
      };

      extraOptions = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = ''
          Extra command-line arguments to pass to `jupyter-notebook`
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      systemd.user.services.jupyter-notebook = {
        Unit = {
          Description = "A service for automatic startup of jupyter notebook";
        };

        Service = {
          Type = "simple";
          ExecStart = escapeShellArgs jupyterArgs;
          Restart = "on-failure";
        };

        Install.WantedBy = [ "default.target" ];
      };

      launchd.agents.jupyter-notebook = {
        enable = true;
        config =
          let
            home = config.home;
          in
          {
            ProgramArguments = jupyterArgs;
            WorkingDirectory = "${home.homeDirectory}";
            Label = "org.nix-community.home.jupyter";
            RunAtLoad = true;
            StandardOutPath = "${home.homeDirectory}/Library/Logs/jupyter.log";
            StandardErrorPath = "${home.homeDirectory}/Library/Logs/jupyter.log";
          };
      };
    })
  ];
}
