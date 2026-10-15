{
  self,
  lib,
  config,
  ...
}:
{
  flake.nixosModules.zed = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [ zed-editor ];
    environment.sessionVariables.EDITOR = "zeditor";
  };
}
