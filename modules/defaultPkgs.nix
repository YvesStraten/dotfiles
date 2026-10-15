{ lib, config, ... }: {
  flake.nixosModules.core = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [
      vim
      nix-prefetch-scripts
      rsync
      btop
    ];
  };
}
