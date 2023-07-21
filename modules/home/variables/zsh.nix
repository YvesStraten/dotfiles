{pkgs, ...}: {
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;

    shellAliases = {
      # neovide = "WINIT_UNIX_BACKEND=x11 neovide";
      # updatenix = "nix flake update ~/Git-repos/dotfiles";
      # upgradenix = "sudo nixos-rebuild switch --flake ~/Git-repos/dotfiles#nitro";
      updatesymlinks = "home-manager switch --flake ~/Git-repos/dotfiles";
      mux = "tmuxinator"
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
        "tmux"
      ];
      theme = "mortalscumbag";
    };
  };

  programs.bash = {
    enable = true;
    profileExtra = ''
      export XDG_DATA_DIRS=\"$HOME/.nix-profile/share:$XDG_DATA_DIRS\"
    '';
  };
}
