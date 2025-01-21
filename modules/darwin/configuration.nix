{
  pkgs,
  user,
  shell,
  ...
}:
{
  imports = [
    ../nixos/settings.nix
  ];

  users.users.${user} = {
    name = user;
    home = "/Users/${user}";
  };

  nixpkgs.config = {
    allowUnfree = true;
  };

  programs.${shell}.enable = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = [
    pkgs.vim
  ];

  services.jankyborders.enable = true;
  services.aerospace = {
    enable = true;
    settings = {
      mode.main.binding =
        let
          workspaces =
            { i }:
            {
              "alt-${builtins.toString i}" = "workspace " + builtins.toString i;
            };

          keys = {
            "h" = "left";
            "j" = "down";
            "k" = "up";
            "l" = "right";
          };

        in
        {
          alt-enter = "exec-and-forget open -na Kitty";
          alt-shift-s = "exec-and-forget open -a Screenshot";
          alt-b = "exec-and-forget open -na Firefox";
          alt-h = "focus left";
          alt-j = "focus down";
          alt-k = "focus up";
          alt-l = "focus right";
          alt-shift-h = "move left";
          alt-shift-j = "move down";
          alt-shift-k = "move up";
          alt-shift-l = "move right";
          alt-1 = "workspace 1";
          alt-2 = "workspace 2";
          alt-3 = "workspace 3";
          alt-4 = "workspace 4";
          alt-5 = "workspace 5";
          alt-6 = "workspace 6";
          alt-7 = "workspace 7";
          alt-8 = "workspace 8";
          alt-9 = "workspace 9";
          alt-shift-1 = "move-node-to-workspace 1";
          alt-shift-2 = "move-node-to-workspace 2";
          alt-shift-3 = "move-node-to-workspace 3";
          alt-shift-4 = "move-node-to-workspace 4";
          alt-shift-5 = "move-node-to-workspace 5";
          alt-shift-6 = "move-node-to-workspace 6";
          alt-shift-7 = "move-node-to-workspace 7";
          alt-shift-8 = "move-node-to-workspace 8";
          alt-shift-9 = "move-node-to-workspace 9";
          alt-ctrl-h = "resize width -5";
          alt-ctrl-l = "resize width +5";
          alt-ctrl-j = "resize height -5";
          alt-ctrl-k = "resize height +5";
          alt-shift-f = "fullscreen";
        };
    };
  };

  system.stateVersion = 5;
  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

  system.activationScripts.extraActivation.text = ''
    sudo ln -sfn "${pkgs.openjdk}/zulu-21.jdk" "/Library/Java/JavaVirtualMachines/"
  '';
}
