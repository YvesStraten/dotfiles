{ pkgs, ... }: {
  imports = [
    # ./alacritty.nix
    ./kitty.nix
    ./emacs/emacs.nix
    # ./nvim/neovim.nix
    # ./vscode.nix
    # ./tmux/tmux.nix
    ./zsh.nix

    ./languages.nix
  ];
}
