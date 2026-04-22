{ self, lib, ... }:
{
  flake.nixosModules.shikane =
    { pkgs, config, ... }:
    {

      environment.systemPackages = with pkgs; [
        shikane
      ];

      systemd.user.services."shikane" = {
        unitConfig = {
          Description = "Shikane daemon";
          After = [ "graphical-session.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        serviceConfig = {
          Type = "simple";
          ExecStart = "${lib.getExe' pkgs.shikane "shikane"}";
          Restart = "always";
          RestartSec = 5;
          KillMode = "process";
        };

        wantedBy = [ "graphical-session.target" ];
      };
    };
}
