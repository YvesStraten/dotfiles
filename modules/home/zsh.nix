{pkgs, ...}: {
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;

    shellAliases = {
      neovide = "WINIT_UNIX_BACKEND=x11 neovide";
      updatenix = "nix flake update ~/Git-repos/dotfiles";
      upgradenix = "sudo nixos-rebuild switch --flake ~/Git-repos/dotfiles#nitro";
    };
    profileExtra = ''
      export XDG_DATA_DIRS=\"$HOME/.nix-profile/share:$XDG_DATA_DIRS\
      export XDG_DATA_DIRS:$XDG_DATA_DIRS:/usr/share:/var/lib/flatpak/exports/share:$HOME/.local/share/flatpak/exports/share
    '';
    initExtraFirst = ''
      ZSH_DISABLE_COMPFIX=true
      ZSH_TMUX_CONFIG="$HOME/.config/tmux/.tmux.conf"
    '';

    oh-my-zsh = {
      enable = true;
      plugins = [
        "git"
        "common-aliases"
        "tmux"
      ];
      theme = "robbyrussell";
    };
  };

  programs.bash = {
    enable = true;
    profileExtra = ''
      export XDG_DATA_DIRS=\"$HOME/.nix-profile/share:$XDG_DATA_DIRS\"
    '';
  };
}
