{ ... }: {
  imports = [
    ./fish.nix
    ./kitty.nix
    ./tmux.nix
    ./vscode.nix
    ./zsh.nix
    ./starship.nix
    ./git.nix
    ./emacs/emacs.nix
    ./languages.nix
    ./jupyter.nix
  ];
}
