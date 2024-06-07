{shell, ...}: {
  imports = [
    # ./alacritty.nix
    ./kitty.nix
    ./emacs/emacs.nix
    # ./nvim/neovim.nix
    # ./vscode.nix
    ./tmux/tmux.nix
    ./${shell}.nix

    ./languages.nix
    ./git.nix
  ];
}
