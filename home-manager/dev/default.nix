{ ... }: {
  imports = [
    ./fish.nix
    ./kitty.nix
    ./ghostty.nix
    ./tmux.nix
    ./neovim.nix
    ./vscode.nix
    ./zsh.nix
    ./starship.nix
    ./git.nix
    ./emacs/emacs.nix
    ./languages.nix
    ./jupyter.nix
    ./zoxide.nix
  ];
}
