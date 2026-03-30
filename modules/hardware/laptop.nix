{ self, ... }:
{
  flake.nixosModules.hardware_laptop = _: {
    imports = with self.nixosModules; [
      hardware_bluetooth
      hardware_power
      hardware_sound
      hardware_networking
    ];
  };
}
