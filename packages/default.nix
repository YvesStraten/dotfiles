{pkgs, ...}: let
  callPackage = pkgs.callPackage;
in {
  sugar-dark = callPackage ./sugar-dark.nix {};
  sekiro = callPackage ./sekiro.nix {};
  nvChad = callPackage ./nvchad.nix {};
  whitesur-cursors = callPackage ./whitesur-cursors.nix {};
  wall-switch = callPackage ./wall-switch.nix {};
	drive = callPackage ./drive.nix {};
}
