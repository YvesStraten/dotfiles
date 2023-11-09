{ pkgs, ... }: {
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    syntaxHighlighting.enable = true;
    enableCompletion = true;

    shellAliases = {
      # neovide = "prime-run nixGLNvidia-535.86.05 neovide";
      updatenix = "nix flake update ~/Git-repos/dotfiles";
      upgradenix = "sudo nixos-rebuild switch --flake ~/Git-repos/dotfiles#nitro";
      wslupgrade = "sudo nixos-rebuild switch --flake ~/Git-repos/dotfiles#wsl";
      # updatesymlinks = "home-manager switch --flake ~/Git-repos/dotfiles";
      mux = "tmuxinator";
      web = "nix develop ~/Git-repos/dotfiles#web --impure";
      cpp = "nix develop ~/Git-repos/dotfiles#cpp --impure";
      arduino = "nix develop ~/Git-repos/dotfiles#arduino --impure";
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
      plugins = [
        "git"
        "common-aliases"
      ];
    };
  };

  programs.bash = {
    enable = true;
    profileExtra = ''
      export XDG_DATA_DIRS=\"$HOME/.nix-profile/share:$XDG_DATA_DIRS\"
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
