{ inputs, ... }:
{
  programs.zathura = {
    enable = true;
    options = {
      adjust-open = "best-fit";
      smooth-scroll = "true";
      selection-clipboard = "clipboard";
    };

    extraConfig = "${builtins.readFile "${inputs.zathura-dracula}/zathurarc" }";
  };
}
