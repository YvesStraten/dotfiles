{pkgs, ...}: {
  programs = {
    password-store = {
      enable = true;
      package = pkgs.pass.withExtensions (exts: with exts; [pass-otp pass-import]);
    };
    gpg.enable = true;
  };
}
