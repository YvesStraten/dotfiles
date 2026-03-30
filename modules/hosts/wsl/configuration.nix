{ inputs, ... }:
{
  flake.nixosModules.wsl =
    {
      config,
      pkgs,
      ...
    }:
    let
      inherit (config.custom.constants) user;
    in
    {
      imports = [
        ../../../overlays/default.nix
        inputs.nixos-wsl.nixosModules.wsl
      ];

      environment.systemPackages = with pkgs; [
        vim
        git
        gh
        yvess.win32yank
      ];

      system.stateVersion = "22.11";
      networking.hostName = "wsl";

      programs.dconf.enable = true;

      wsl = {
        enable = true;
        wslConf.automount.root = "/mnt";
        defaultUser = "${user}";

        startMenuLaunchers = true;
      };
    };
}
