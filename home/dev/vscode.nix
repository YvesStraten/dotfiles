{pkgs, ...}: {
  programs.vscode = {
    enable = true;
    extensions = with pkgs.vscode-extensions; [
      vscodevim.vim
      dracula-theme.theme-dracula
      jebbs.plantuml
      ban.spellright
      ms-dotnettools.csdevkit
      mkhl.direnv
      jnoortheen.nix-ide
      rust-lang.rust-analyzer
      james-yu.latex-workshop
      
      bbenoist.nix

    ] ++ (with ms-vscode; [
      live-server
      cpptools-extension-pack

    ]);
  };
}
