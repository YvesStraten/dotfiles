{pkgs, ...}: {
  services.alt-tab.enable = if pkgs.stdenv.isDarwin then true else false;
}
