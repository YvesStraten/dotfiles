{pkgs, ...}: {
  home.sessionVariables = {
    EDITOR = "code --wait --new-window";
  };
  programs.vscode = {
    enable = true;
    extensions = with pkgs.vscode-extensions; [
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
    ] ++ (with ms-vscode; [
      live-server
      cpptools-extension-pack

    ]);
  };
}
