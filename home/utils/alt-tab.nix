{pkgs, ...}: {
  imports = [
    ../../config/alt-tab/alt-tab.nix
  ];

  services.alt-tab.enable = true;
}
