{
  self,
  lib,
  inputs,
  ...
}:
{
  flake.nixosModules.kvantum = { pkgs, config, ... }: {
    environment.systemPackages = with pkgs; [
      qt6Packages.qt6ct
      qt6Packages.qtstyleplugin-kvantum
      rose-pine-kvantum
    ];

    qt = {
      enable = true;
      style = "kvantum";
      platformTheme = "qt5ct";
    };
  };
}
