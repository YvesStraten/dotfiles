{
  self,
  pkgs,
  ...
}: {
  nix.package = pkgs.nixFlakes;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  system.stateVersion = "22.11";
  networking.hostName = "wsl";

  programs.zsh.enable = true;

  users = {
    defaultUserShell = pkgs.zsh;
    users.akali = {
      isNormalUser = true;
      description = "akali";
      extraGroups = ["wheel" "docker"];
    };
  };

  wsl = {
    enable = true;
    wslConf.automount.root = "/mnt";
    defaultUser = "akali";

    startMenuLaunchers = true;
  };
}
