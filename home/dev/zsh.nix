{pkgs, lib, config, ...}: {
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    enableCompletion = true;

    shellAliases = {
      cat = "${lib.getExe pkgs.bat} \"$@\"";
    };
    profileExtra = ''
      export QT_QPA_PLATFORMTHEME=qt5ct
    '';
    initExtraFirst = ''
      ZSH_DISABLE_COMPFIX=true
    '';

    oh-my-zsh = {
      enable = true;
      plugins = ["git" "common-aliases"];
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
