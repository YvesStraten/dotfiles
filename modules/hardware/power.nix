{
  flake.nixosModules.hardware_power =
    { ... }:
    {
      services = {
        thermald.enable = true;
        tlp.enable = true;
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
