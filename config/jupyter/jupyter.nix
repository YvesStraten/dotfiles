{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.services.jupyter-notebook;
  defaultJupyterArgs = [ "${lib.getExe pkgs.jupyter}" "--no-browser" ];

  jupyterArgs = defaultJupyterArgs ++ cfg.extraOptions;
in {
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

      launchd.agents.jupyter-notebook = with config.home; {
        enable = true;
        config = {
          ProgramArguments = jupyterArgs;
          WorkingDirectory = "${homeDirectory}";
          Label = "org.nix-community.home.jupyter";
          RunAtLoad = true;
          StandardOutPath = "${homeDirectory}/Library/Logs/jupyter.log";
          StandardErrorPath = "${homeDirectory}/Library/Logs/jupyter.log";
        };
      };
    })
  ];
}
