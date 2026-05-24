{ self, ... }:
{
  flake.homeModules.deck =
    {
      pkgs,
      lib,
      ...
    }:
    {
      imports = with self.homeModules; [
        utils
        dev
      ];

      custom = {
        fish.enable = true;
        tmux.enable = true;
        ghostty.enable = true;
        firefox.enable = true;
        utils.enable = lib.mkForce false;
      };

      home.packages = [
        pkgs.wl-clipboard
      ];
    };
}
