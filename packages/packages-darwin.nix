{ pkgs, ... }: {
  skim = pkgs.callPackage ./skim.nix { };
}
