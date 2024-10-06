{ pkgs, lib, ... }: let
  inherit (lib) getExe;
in {
programs.helix = {
  enable = true;
  settings = {
    theme = "autumn_night_transparent";
    editor.cursor-shape = {
      normal = "block";
      insert = "bar";
      select = "underline";
    };
  };
  languages.language = with pkgs; [{
    name = "nix";
    auto-format = true;
    formatter.command = "${getExe nixfmt-rfc-style}";
  }];
  themes = {
    autumn_night_transparent = {
      "inherits" = "autumn_night";
      "ui.background" = { };
    };
  };
};
}
