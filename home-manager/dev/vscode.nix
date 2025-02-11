{
  config,
  options,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.custom.vscode;
  inherit (lib) mkMerge mkEnableOption mkIf;
in
{
  options.custom.vscode.enable = mkEnableOption "Enable vscode";

  config = mkIf cfg.enable {
    programs.vscode = {
      enable = true;
      extensions =
        with pkgs.vscode-extensions;
        [
          vscodevim.vim
          dracula-theme.theme-dracula
          jebbs.plantuml
          ban.spellright
          ms-dotnettools.csdevkit
          ms-dotnettools.csharp
          mkhl.direnv
          jnoortheen.nix-ide
          rust-lang.rust-analyzer
          james-yu.latex-workshop

          bbenoist.nix
          streetsidesoftware.code-spell-checker
        ]
        ++ (with ms-vscode; [
          live-server
          cpptools-extension-pack
        ]);
    };
  };
}
