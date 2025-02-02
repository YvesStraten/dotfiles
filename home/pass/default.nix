{pkgs, ...}: {
  programs = {
    password-store = {
      enable = true;
      package = pkgs.pass.withExtensions (exts: with exts; [pass-otp pass-import pass-update]);
    };
    gpg.enable = true;
  };

  services.gpg-agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-gnome3;
  };
}
