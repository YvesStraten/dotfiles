{
  config,
  pkgs,
  ...
}: {
  home = {
    sessionVariables = {
      EDITOR = "emacs";
      TERMINAL = "kitty";
    };
  };
}
