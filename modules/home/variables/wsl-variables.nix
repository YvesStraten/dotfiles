{
  config,
  pkgs,
  ...
}: {
  home = {
    sessionVariables = {
      TERMINAL = "kitty";
    };
  };
}
