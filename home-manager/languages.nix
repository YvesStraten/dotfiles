{ pkgs, ...}:
{
    home.packages = with pkgs; [
      nodejs
      yarn
      gcc
      omnisharp-roslyn
    ];
  }
