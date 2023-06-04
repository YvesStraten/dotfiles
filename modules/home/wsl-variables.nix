{
  config,
  pkgs,
  ...
}: {
  home = {
    sessionVariables = {
      EDITOR = "nvim";
      TERMINAL = "kitty";
    };
  };
}
