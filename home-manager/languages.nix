{ pkgs, ...}:
{
    home.packages = with pkgs; [
      nodejs
      yarn
      python311
      gcc
      omnisharp-roslyn
    ];
  }
