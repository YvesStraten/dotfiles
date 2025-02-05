{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    types
    mkOption
    mkMerge
    mkIf
    ;
  cfg = config.services.rclone-bisync;
  bisyncType = types.attrsOf (
    types.submodule {
      options = {
        remotePath = mkOption {
          type = types.str;
          default = "";
        };

        localPath = mkOption {
          type = types.path;
          default = "";
        };

        extraArgs = mkOption {
          type = types.listOf types.str;
          default = [ "--verbose" ];
        };

        timeDelay = mkOption {
          type = types.int;
          default = 300;
        };
      };
    }
  );

  mkBisyncService =
    {
      name,
      bisyncCommand,
      remotePath,
      localPath,
    }:
    {
      "rclone-${name}" = {
        Unit = {
          Description = "Bisync service for ${remotePath} to ${localPath}";
          After = "network-online.target";
          Wants = "network-online.target";
        };
        Service = {
          Type = "simple";
          ExecStart = lib.escapeShellArgs (bisyncCommand);
          OnFailure = "rclone-${name}-recovery.service";
        };

        Install = {
          WantedBy = [ "default.target" ];
        };
      };

      "rclone-${name}-recovery" =
        let
          failureScript = pkgs.writeShellScriptBin "recover" ''
            ${lib.getExe pkgs.libnotify} "Sync failed" "Sync failed from ${remotePath} to ${localPath}, resyncing"
            ${lib.escapeShellArgs (bisyncCommand ++ [ "--resync" ])}
          '';
        in
        {
          Unit = {
            Description = "Recovery service for bisync ${remotePath} to ${localPath}";
            After = "network-online.target";
            Wants = "network-online.target";
          };
          Service = {
            Type = "simple";
            ExecStart = "${lib.getExe failureScript}";
          };

          Install = {
            WantedBy = [ "default.target" ];
          };
        };
    };

  mkBisyncTimer =
    {
      name,
      timeOut,
    }:
    let
      delay = timeOut / 60;
    in
    {
      "rclone-${name}" = {
        Unit = {
          Description = "Timer for rclone-${name}.service";
          Requires = "rclone-${name}.service";
        };
        Timer = {
          OnCalendar = "*:0/${toString delay}";
          Unit = "rclone-${name}.service";
        };

        Install = {
          WantedBy = [ "timers.target" ];
        };
      };
    };

  mkLaunchdService =
    {
      remotePath,
      localPath,
      timeOut,
      name,
      args,
    }:
    {
      enable = true;
      config =
        let
          homeDirectory = config.home.homeDirectory;
        in
        {
          ProgramArguments = [
            "${lib.getExe pkgs.rclone}"
            "bisync"
            "${remotePath}"
            "${localPath}"
          ] ++ args;
          WorkingDirectory = "${homeDirectory}";
          Label = "org.rclone.${name}";
          RunAtLoad = true;

          StandardOutPath = "/tmp/rclone-${name}.log";
          StandardErrorPath = "/tmp/rclone-${name}.log";

          StartInterval = timeOut;
        };
    };
in
{
  options = {
    services.rclone-bisync = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
      services.enable = mkOption {
        type = types.bool;
        default = false;
      };
      bisyncs = mkOption {
        type = bisyncType;
        default = { };
        description = "Your bisyncs";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.bisyncs != { }) {
      home.packages = [ pkgs.rclone ];

      systemd.user.services = lib.attrsets.concatMapAttrs (
        name: bisync:
        let
          bisyncCommand = [
            "${lib.getExe pkgs.rclone}"
            "bisync"
            "${bisync.localPath}"
            "${bisync.remotePath}"
          ] ++ bisync.extraArgs;
        in

        mkBisyncService {
          inherit bisyncCommand name;
          localPath = bisync.localPath;
          remotePath = bisync.remotePath;
        }
      ) cfg.bisyncs;

      systemd.user.timers = lib.attrsets.concatMapAttrs (
        name: bisync:
        mkBisyncTimer {
          name = "${name}";
          timeOut = bisync.timeDelay;
        }
      ) cfg.bisyncs;

      launchd.agents = builtins.mapAttrs (
        name: bisync:
        mkLaunchdService {
          remotePath = "${bisync.remotePath}";
          localPath = "${bisync.localPath}";
          args = bisync.extraArgs;
          name = "${name}";
          timeOut = bisync.timeDelay;
        }
      ) cfg.bisyncs;
    })
  ];
}
