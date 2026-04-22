{
  flake.nixosModules.hardware_power =
    { ... }:
    {
      services = {
        thermald.enable = true;
        power-profiles-daemon.enable = true;
        upower = {
          enable = true;
          criticalPowerAction = "Hibernate";
        };

        logind.settings.Login = {
          HandlePowerKey = "ignore";
        };
      };
    };
}
