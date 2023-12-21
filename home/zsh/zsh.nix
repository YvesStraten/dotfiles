{ pkgs, ... }: {
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    syntaxHighlighting.enable = true;
    enableCompletion = true;

    shellAliases = {
      updatenix = "nix flake update ~/dotfiles";
      # upgradenix = "sudo nixos-rebuild switch --flake ~/dotfiles#nitro";
      updatesymlinks = "home-manager switch --flake ~/dotfiles#akali";
      mux = "tmuxinator";
      web = "nix develop ~/dotfiles#web --impure";
      c = "nix develop ~/dotfiles#c --impure";
      arduino = "nix develop ~/dotfiles#arduino --impure";
      cat = "${pkgs.bat}/bin/bat";
    };
    profileExtra = ''
      export QT_QPA_PLATFORMTHEME=qt5ct
    '';
    initExtraFirst = ''
      ZSH_DISABLE_COMPFIX=true
    '';

    oh-my-zsh = {
      enable = true;
      plugins = [ "git" "common-aliases" ];
    };
  };

  programs.bash = {
    enable = true;
    profileExtra = ''
            export XDG_DATA_DIRS=\"$HOME/.nix-profile/share:$XDG_DATA_DIRS\"
      			. ~/.nix-profile/etc/profile.d/nix.sh
    '';
  };

  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    settings = {
      add_newline = false;
      hostname = {
        ssh_only = false;
        disabled = false;
      };
    };
  };
}
