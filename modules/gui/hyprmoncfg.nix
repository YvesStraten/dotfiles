{ self, lib, ... }:
{
  flake.nixosModules.hyprmoncfg =
    { pkgs, config, ... }:
    {

      environment.systemPackages = with pkgs; [
        hyprmoncfg
      ];

      systemd.user.services."hyprmoncfg" = {
        unitConfig = {
          Description = "Hyprmoncfg daemon";
          After = [ "graphical-session.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        serviceConfig = {
          Type = "simple";
          ExecStart = "${lib.getExe' pkgs.hyprmoncfg "hyprmoncfgd"}";
          Restart = "always";
          RestartSec = 5;
          KillMode = "process";
        };

        wantedBy = [ "graphical-session.target" ];
      };
    };
}
