{ config, lib, pkgs, ... }:
let
  inherit (lib) types mkOption mkMerge mkIf;
  cfg = config.services.rclone-bisync;
  bisyncType = types.attrsOf (types.submodule {
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
  });

  mkBisyncService = { remotePath, localPath, args }:
    let
      failureScript = pkgs.writeShellScriptBin "recover" ''
        if [ "$SERVICE_RESULT" = 'exit-code' ];
        then ${lib.getExe pkgs.libnotify} "Sync failed" "Sync failed from ${remotePath} to ${localPath}";
        fi
      '';
    in {
      Unit = {
        Description = "Bisync service for ${remotePath} to ${localPath}";
      };
      Service = {
        Type = "oneshot";
        ExecStart = lib.escapeShellArgs ([
          "${lib.getExe pkgs.rclone}"
          "bisync"
          "${localPath}"
          "${remotePath}"
        ] ++ args);

        ExecStopPost = "${lib.getExe failureScript}";
      };

      Install = { WantedBy = [ "default.target" ]; };
    };

  mkBisyncTimer = { name, timeOut }:
    let
    delay = timeOut / 60;
  in {
    Unit = { Description = "Timer for ${name}.service"; };
    Timer = {
      OnCalendar = "*:0/${toString delay}";
      Unit = "${name}.service";
    };

    Install = { WantedBy = [ "default.target" ]; };
  };

  # mkLaunchdService = { remotePath, localPath, timeOut, name, args }:{
  #     enable = true;
  #     config = let
  #       homeDirectory = config.home.homeDirectory;
  #     in{
  #       ProgramArguments = [
  #         "${lib.getExe pkgs.rclone}"
  #         "bisync"
  #         "${localPath}"
  #         "${remotePath}"
  #       ] ++ args;
  #       WorkingDirectory = "${homeDirectory}";
  #       Label = "org.rclone.${name}";
  #       RunAtLoad = true;

  #       StandardOutPath =
  #         "${homeDirectory}/Library/Logs/rclone-${name}.log";
  #       StandardErrorPath =
  #         "${homeDirectory}/Library/Logs/rclone-${name}.log";

  #       StartInterval = 300;
  #     };
  #   };

in {
  options = {
    services.rclone-bisync = {
      enable = mkOption {
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

      systemd.user.services = builtins.mapAttrs (name: bisync:
        mkBisyncService {
          remotePath = "${bisync.remotePath}";
          localPath = "${bisync.localPath}";
          args = bisync.extraArgs;
        }) cfg.bisyncs;

      # systemd.user.timers =
      #   builtins.mapAttrs (name: bisync: mkBisyncTimer { name = "${name}"; timeOut = bisync.timeDelay; })
      #   cfg.bisyncs;

      # launchd.agents = builtins.mapAttrs (name: bisync:
      #   mkLaunchdService {
      #     remotePath = "${bisync.remotePath}";
      #     localPath = "${bisync.localPath}";
      #     args = bisync.extraArgs;
      #     name = "${name}";
      #   }) cfg.bisyncs;
    })
  ];
}
