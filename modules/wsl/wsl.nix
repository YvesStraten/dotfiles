{ pkgs, user, shell, ... }: {
  imports = [
    ../../overlays/default.nix
    ../nixos/settings.nix
  ];

  environment.systemPackages = with pkgs; [
    vim
    git
    gh
    yvess.win32yank
  ];

  system.stateVersion = "22.11";
  networking.hostName = "wsl";

  programs.${shell}.enable = true;

  users = {
    defaultUserShell = pkgs.${shell};
    users.${user} = {
      isNormalUser = true;
      description = "${user}";
      extraGroups = [ "wheel" "docker" ];
    };
  };

  programs.dconf.enable = true;

  wsl = {
    enable = true;
    wslConf.automount.root = "/mnt";
    defaultUser = "${user}";
    nativeSystemd = true;

    startMenuLaunchers = true;
  };
}
