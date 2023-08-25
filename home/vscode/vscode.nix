{ pkgs, ... }: {
  programs.vscode = {
    enable = true;
    extensions = with pkgs; [

    ];
  };

  nixpkgs.config.allowUnfree = true;
}
