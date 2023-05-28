{ pkgs, ...}:
{
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    profileExtra = ''
      export XDG_DATA_DIRS=\"$HOME/.nix-profile/share:$XDG_DATA_DIRS\
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
