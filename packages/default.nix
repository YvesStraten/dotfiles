{ pkgs, nvim, ... }: {
  default = nvim;
  nvim = nvim;
  sugar-dark = pkgs.callPackage ./sugar-dark.nix { };
  sekiro = pkgs.callPackage ./sekiro.nix { };
  whitesur-cursors = pkgs.callPackage ./whitesur-cursors.nix { };
  wall-switch = pkgs.callPackage ./wall-switch.nix { };
  skim = pkgs.callPackage ./skim.nix { };
  alt-tab = pkgs.callPackage ./alt-tab.nix { };
  win32yank = pkgs.callPackage ./win32yank.nix { };
  theme = pkgs.callPackage ./theme.nix { };
}
