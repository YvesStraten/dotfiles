{ pkgs, ... }:
{
  sugar-dark = pkgs.callPackage ./sugar-dark.nix { };
  sekiro = pkgs.callPackage ./sekiro.nix { };
  wall-switch = pkgs.callPackage ./wall-switch.nix { };
  win32yank = pkgs.callPackage ./win32yank.nix { };
}
