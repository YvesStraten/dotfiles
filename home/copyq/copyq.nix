{pkgs, ...}: {
  services.copyq = {
    enable = true;
    systemdTarget = "sway-session.target";
  };
}
