{ self, ... }:
{
  flake.homeModules.wsl = _: {
    imports = with self.homeModules; [
      dev
      theming
    ];

    custom = {
      fish.enable = true;
      theming.enable = true;
      languages.enable = true;
      tmux.enable = true;
    };
  };
}
