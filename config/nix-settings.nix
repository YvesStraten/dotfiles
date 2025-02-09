{
  config,
  options,
  lib,
  user,
  inputs,
  ...
}:
let
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  config = mkMerge [
    {
      nix = {
        settings.experimental-features = [
          "nix-command"
          "flakes"
        ];
        nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];

        settings.trusted-users = [
          "root"
          user
        ];

        # Allow unfree packages
      };

      nixpkgs.config.allowUnfree = true;
    }
  ];
}
