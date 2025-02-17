{
  config,
  options,
  lib,
  ...
}:
let
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  config = mkMerge [
    {
      xdg.userDirs = {
        enable = true;
        createDirectories = true;
      };
    }
  ];
}
