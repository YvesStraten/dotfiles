{ self
, pkgs
, ...
}: {
  imports = [
    ../../overlays/default.nix
  ];

  nix = {
    settings = {
      trusted-users = [ "akali" ];
      experimental-features = [ "nix-command" "flakes" ];
    };
    package = pkgs.nixFlakes;
  };

  environment.systemPackages = with pkgs; [
    vim
    git
    gh
  ];

  system.stateVersion = "22.11";
  networking.hostName = "wsl";

  programs.zsh.enable = true;

  users = {
    defaultUserShell = pkgs.zsh;
    users.akali = {
      isNormalUser = true;
      description = "akali";
      extraGroups = [ "wheel" "docker" ];
    };
  };

  programs.dconf.enable = true;

  wsl = {
    enable = true;
    wslConf.automount.root = "/mnt";
    defaultUser = "akali";
    nativeSystemd = true;

    startMenuLaunchers = true;
  };
}
